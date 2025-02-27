/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT

import { Ymacs_Buffer } from "./ymacs-buffer.js";
import { Ymacs_Tokenizer, Ymacs_Simple_Stream } from "./ymacs-tokenizer.js";
import { Ymacs_Keymap } from "./ymacs-keymap.js";
import { Ymacs_Interactive } from "./ymacs-interactive.js";
import { toHash, regexp_opt, Cons, NIL } from "./ymacs-utils.js";
import { Ymacs_BaseLang } from "./ymacs-baselang.js";

(function(){

    function Partial(value) {
        this.value = value;
    }

    function QuickParser(buffer, pos) {
        var input = new Ymacs_Simple_Stream({ buffer: buffer, pos: pos });
        function peek() { return input.peek() }
        function next() { return input.next() }
        function skip_ws() {
            return input.read_while(function(ch){
                if (!caret_token && caret != null && input.pos == caret) return false;
                return input.is_whitespace(ch);
            });
        }
        function skip(ch) { next() }
        function read_while(pred) { return input.read_while(pred) }
        function read_escaped(start, end, inces) {
            skip(start);
            var escaped = false;
            var str = "";
            while (true) {
                var ch = next();
                if (!ch) throw new Partial(str);
                if (escaped) {
                    str += ch;
                    escaped = false;
                } else if (ch == "\\") {
                    if (inces) str += ch;
                    escaped = true;
                } else if (ch == end) {
                    break;
                } else {
                    str += ch;
                }
            }
            return str;
        }
        function read_comment() {
            return read_while(function(ch){ return ch && ch != "\n" });
        }
        function read_multiline_comment() {
            var comment = read_while(function(){
                return !input.looking_at("|#");
            });
            skip(); skip();
            return comment;
        }
        function read_string() {
            return read_escaped("\"", "\"");
        }
        function read_list(beg, end) {
            var save_list_index = list_index;
            list_index = 0;
            try {
                var ret = [], p;
                skip(beg);
                out: while (true) {
                    skip_ws();
                    switch (peek()) {
                      case end: break out;
                      case null: throw new Partial(ret);
                      default:
                        ret.push(read_token());
                        ++list_index;
                    }
                }
                skip(end);
                return ret;
            } finally {
                list_index = save_list_index;
            }
        }
        function read_regexp() {
            var str = read_escaped("/", "/", true);
            var mods = read_while(function(ch){
                if (ch) switch (ch.toLowerCase()) {
                  case "y":
                  case "m":
                  case "g":
                  case "i":
                    return true;
                }
            }).toLowerCase();
            return { pattern: str, modifiers: mods };
        }
        function is_symbol_char(ch) {
            switch (ch) {
              case null:
              case "(":
              case ")":
              case "{":
              case "}":
              case "[":
              case "]":
              case "#":
              case ";":
              case "`":
              case "'":
              case "\"":
              case "|":
              case " ":
              case "\n":
              case "\t":
              case "\x0C":
              case "\u2028":
              case "\u2029":
              case "\xA0":
                return false;
            }
            return true;
        }
        function read_symbol() {
            return read_while(is_symbol_char);
        }
        function read_char() {
            return next() + read_while(function(ch){
                return (ch >= "a" && ch <= "z") ||
                    (ch >= "A" && ch <= "z") ||
                    (ch >= "0" && ch <= "9") ||
                    ch == "-" || ch == "_";
            });
        }
        function read_elisp_char() {
            skip("?");
            if (peek() == "\\") next();
            return next();
        }
        function read_sharp() {
            skip("#");
            switch (peek()) {
              case "\\": next(); return token("char", read_char);
              case "/": return token("regexp", read_regexp);
              case "(": return token("vector", read_list.bind(null, "(", ")"));
              case "'": next(); return token("function", read_token);
              case "|": next(); return token("comment", read_multiline_comment);
              default:
                return token("unknown", read_token);
            }
        }
        function read_token() {
            skip_ws();
            if (!caret_token && caret != null && input.pos == caret && (!parent || parent.type == "list")) {
                return caret_token = token("caret");
            }
            switch (peek()) {
              case ";"  : return token("comment", read_comment);
              case "\"" : return token("string", read_string);
              case "("  : return token("list", read_list.bind(null, "(", ")"));
              case "{"  : return token("list", read_list.bind(null, "{", "}"));
              case "["  : return token("list", read_list.bind(null, "[", "]"));
              case "#"  : return token("sharp", read_sharp);
              case "?"  : return token("char", read_elisp_char);
              case "`"  : next(); return token("qq", read_token, -1);
              case ","  :
                next();
                if (peek() == "@") {
                    next();
                    return token("splice", read_token, -2);
                }
                return token("unquote", read_token, -1);
              case "'"  : next(); return token("quote", read_token, -1);
              case ")"  : return null;
              case null : return null; // EOF
            }
            return token("symbol", read_symbol);
        }
        function read_all() {
            var ret = [];
            while (peek() != null) {
                var tok = read_token();
                if (tok == null) break;
                ret.push(tok);
                ++list_index;
            }
            return ret;
        }
        var caret_token = null;
        var list_index = 0;
        var caret = null;
        var parent = null;
        var cont_exp = null;
        function token(type, reader, adjust_start) {
            if (adjust_start == null) adjust_start = 0;
            var save_parent = parent;
            try {
                var tok = {
                    value   : null,
                    index   : list_index,
                    type    : type,
                    start   : input.pos + adjust_start,
                    parent  : parent,
                    depth   : parent ? parent.depth + 1 : 0,
                    partial : false
                };
                if (type == "list") parent = tok;
                try {
                    if (reader) {
                        tok.value = reader();
                        if (tok.value === "" && type != "string") {
                            // couldn't figure this out, but let's not crash the browser.
                            next();
                        }
                    }
                } catch(ex) {
                    if (ex instanceof Partial) {
                        tok.value = ex.value;
                        tok.partial = true;
                    } else {
                        throw ex;
                    }
                }
                tok.end = input.pos;
                if (caret != null) {
                    if (tok.start <= caret && tok.end >= caret) {
                        if (!cont_exp) cont_exp = tok;
                    }
                }
                return tok;
            } finally {
                parent = save_parent;
            }
        }
        return {
            parse: function(pos) {
                caret = pos;
                return token("list", read_all);
            },
            read: function() {
                return read_token();
            },
            prev_exp: function() {
                if (caret_token) {
                    return caret_token.parent.value[caret_token.index - 1];
                } else if (cont_exp) {
                    if (cont_exp.type == "list" && cont_exp.end == caret + 1)
                        return cont_exp.value.at(-1);
                    return cont_exp.parent.value[cont_exp.index];
                }
            },
            caret_token: function() {
                return caret_token;
            },
            cont_exp: function() {
                return cont_exp;
            },
            sexp: function() {
                var tok = cont_exp;
                while (tok && (!/^(?:list|string)$/.test(tok.type) || tok.end == caret))
                    tok = tok.parent;
                return tok;
            }
        };
    }

    Ymacs_Buffer.newCommands({

        test_lisp_parse: Ymacs_Interactive(function(){
            try {
                var p = QuickParser(this);
                var ast = p.parse(this.point());
                console.log(ast);
                console.log(p.caret_token());
                console.log(p.cont_exp());
                console.log(p.prev_exp());
            } catch(ex) {
                console.log(ex);
            }
        }),

        lisp_make_quick_parser: function() {
            return QuickParser.apply(this, [this, ...arguments]);
        },

        lisp_forward_sexp: Ymacs_Interactive(function(){
            var p = QuickParser(this, this.point());
            var tok = p.read();
            if (tok) this.cmd("goto_char", tok.end);
        }),

        lisp_backward_sexp: Ymacs_Interactive(function(){
            var p = QuickParser(this);
            p.parse(this.point());
            var tok = p.prev_exp();
            if (tok) this.cmd("goto_char", tok.start);
        }),

        lisp_backward_up_list: Ymacs_Interactive(function(){
            var p = QuickParser(this);
            p.parse(this.point());
            var list = p.sexp();
            if (list && list.parent) this.cmd("goto_char", list.start);
        }),

        lisp_handle_string_quote: Ymacs_Interactive(function(){
            var p = QuickParser(this);
            p.parse(this.point());
            var tok = p.prev_exp();
            if (tok && tok.type == "string" && this.point() < tok.end) {
                if (this.cmd("looking_at", /\"/y))
                    this.cmd("forward_char");
                else if (!this.cmd("looking_back", /\\/g))
                    this.cmd("insert", "\\\"");
                else
                    this.cmd("insert", "\"");
            } else {
                this.cmd("insert", '""');
                this.cmd("backward_char");
            }
        })

    });

})();

const SPECIAL_FORMS = regexp_opt("\
  define defvar defparameter declaim proclaim declare type inline \
  optimize speed safety space debug defconstant \
  deftype defstruct defclass defsetf destructuring-bind \
  defmacro defun defmethod defgeneric defpackage in-package defreadtable in-readtable \
  when cond unless [ec]?typecase e?case \
  lambda λ let load-time-value quote macrolet \
  progn begin prog1 prog2 progv go flet the \
  if throw eval-when multiple-value-prog1 multiple-value-bind unwind-protect let\\* \
  ignore-errors handler-case handler-bind invoke-restart restart-case restart-bind \
  labels function symbol-macrolet block tagbody catch locally \
  inc! dec! cons c[ad]{1,4}r list list\\* eq eql equal equalp and or not null null\\? \
  loop do while dotimes \
  return return-from setq set! set-car! set-cdr! setf multiple-value-call values", "i");

const LOOP_KEYWORDS = regexp_opt("\
  for with and = as in on of then across by while until \
  from downfrom upfrom to upto below downto above \
  being each the hash-keys? using hash-values? \
  collect(?:ing)? nconc(?:ing)? sum(?:ming)? append(?:ing)? into \
  minimize minimizing maximize maximizing \
  named \
  of-type \
  repeat finally \
  if else when unless do");

const ERROR_FORMS = toHash("error warn assert");

const CONSTANTS = toHash("t nil");

const DEFINES_FUNCTION = toHash("defun defmacro defgeneric defmethod");

const DEFINES_TYPE = toHash("deftype defclass defstruct");

const DEFINES_VARIABLE = toHash("defvar defparameter defconstant defconst");

const FORM_ARGS = {
    "if"                  : "3+",
    "aif"                 : "3+",
    "when"                : "1*",
    "awhen"               : "1*",
    "once-only"           : "1*",
    "lambda"              : "1*",
    "unless"              : "1*",
    "defun"               : "2*",
    "defpackage"          : "1*",
    "defgeneric"          : "2*",
    "defmethod"           : "2*",
    "defclass"            : "2*",
    "defstruct"           : "1*",
    "defmacro"            : "2*",
    "progn"               : "0+",
    "begin"               : "0+",
    "prog1"               : "1*",
    "prog2"               : "2*",
    "let"                 : "1*",
    "labels"              : "1*",
    "flet"                : "1*",
    "macrolet"            : "1*",
    "symbol-macrolet"     : "1*",
    "destructuring-bind"  : "2*",
    "unwind-protect"      : "1*",
    "catch"               : "1*",
    "case"                : "1*",
    "ecase"               : "1*",
    "typecase"            : "1*",
    "etypecase"           : "1*",
    "cond"                : "0+",
    "handler-bind"        : "1*",
    "handler-case"        : "1*",
    "restart-bind"        : "1*",
    "restart-case"        : "1*",
    "return-from"         : "1*",
    "block"               : "1*",
    "dotimes"             : "1*",
    "dolist"              : "1*",
    "multiple-value-bind" : "2*",
    ":method"             : "1*",
    "eval-when"           : "1*",
};

const LOCAL_BODYDEF = toHash("labels flet macrolet");

export class Ymacs_Lang_Lisp extends Ymacs_BaseLang {
    _formStack = NIL;
    _formSym = null;
    _formLen = 0;
    _rxSpecial = null;

    COMMENT = [ ";", [ "#|", "|#" ] ];
    STRING = [ '"' ];
    NUMBER = /^[+-]?(?:#x[0-9a-fA-F]+|#\d+r[0-9a-zA-Z]+|#o[0-7]+|#b[01]+|(?:\d*\.)?\d+(?:[eE][+-]?\d+)?|\d+\/\d+)$/u;
    NAME = /^[-_$\p{L}0-9|!#$%&*+./:<=>?@\^~]+/iu;
    OPEN_PAREN = {
        "(" : ")",
        "{" : "}",
        "[" : "]",
        "❰" : "❱",
        "«" : "»",
    };
    CLOSE_PAREN = {
        ")" : "(",
        "}" : "{",
        "]" : "[",
        "❱" : "❰",
        "»" : "«",
    };

    constructor({ stream, tok, rx_special }) {
        super({ stream, tok });
        this._rxSpecial = rx_special;
    }

    isNameChar(ch) {
        return this.NAME.test(ch) && !this._rxSpecial?.test(ch);
    }
    readName() {
        if (!this._rxSpecial) return super.readName();
        let s = this._stream, name = s.peek();
        if (this.isNameChar(name)) {
            let col = s.col++, ch;
            while (this.isNameChar(ch = s.peek())) {
                name += ch;
                s.col++;
            }
            return { line: s.line, c1: col, c2: s.col, id: name };
        }
    }

    copy() {
        let _super = super.copy();
        let _formStack = this._formStack;
        let _formSym = this._formSym;
        let _formLen = this._formLen;
        let _rxSpecial = this._rxSpecial;
        return () => {
            let self = _super();
            self._formStack = _formStack;
            self._formSym = _formSym;
            self._formLen = _formLen;
            self._rxSpecial = _rxSpecial;
            return self;
        };
    }

    newArg(arg) {
        if (arg?.id && !this._formLen) this._formSym = arg;
        this._formLen++;
    }

    isForm(form) {
        var f = this._formSym && this._formSym.id;
        if (f) {
            f = f.toLowerCase();
            if (form == null) return f;
            return typeof form == "string" ? f == form
                :  form instanceof RegExp ? form.test(f)
                :  f in form;
        }
    }

    readCustom() {
        let s = this._stream, m;
        if ((m = s.lookingAt(/^#\\.[a-z0-9_-]*/i))) {
            this.newArg();
            this.t("constant", m[0].length);
            return true;
        }
        if (s.lookingAt("#:") && this.isNameChar(s.peek(+2))) {
            this.newArg();
            s.col += 2;
            this.maybeName("lisp-keyword");
            return true;
        }
        if (s.lookingAt("#'") && this.isNameChar(s.peek(+2))) {
            this.newArg();
            s.col += 2;
            let c = this.maybeName("function-name");
            return true;
        }
        if ((m = s.lookingAt(/^#\/((?:\\.|[^\/])*)\/([dgimsuvy]+)?/))) {
            this.newArg();
            this.t("regexp-starter", 2);
            this.t("regexp", m[1].length);
            this.t("regexp-stopper");
            if (m[2]) this.t("regexp-modifier", m[2].length);
            return true;
        }
        if ((m = s.lookingAt(/^\?(?:\\?.)/u))) {
            // elisp char syntax
            this.newArg();
            this.t("constant", m[0].length);
            return true;
        }
        if ((m = this.readName())) {
            let ch = m.id.charAt(0);
            var type = ch == ":" ? "lisp-keyword"
                : ch == "&" ? "type"
                : m.id in ERROR_FORMS ? "error"
                : m.id in CONSTANTS ? "constant"
                : this.NUMBER.test(m.id) ? "number"
                : null;
            if (!type && this._formStack.car == 2) {
                let pform = this._formStack.cdr?.car?.id;
                if (pform == "define" && this._formLen == 0) {
                    type = "function-name";
                }
                else if (/^def(?!un|ine)/.test(pform)) {
                    type = "function-name";
                }
            }
            if (!type) {
                if (this._formLen == 0 && SPECIAL_FORMS.test(m.id)) {
                    type = "keyword";
                }
                else if (this._formLen == 0 && /^(?::?with(out)?[-\x2f]|def)/i.test(m.id)) {
                    type = "keyword";
                }
                else if (this._formLen == 1 && this.isForm(DEFINES_FUNCTION)) {
                    type = "function-name";
                }
                else if (this._formLen == 1 && this.isForm(DEFINES_TYPE)) {
                    type = "type";
                }
                else if (this._formLen == 1 && this.isForm(DEFINES_VARIABLE)) {
                    type = "variable-name";
                }
                else if (this._formLen == 1 && this.isForm(/^def/)) {
                    type = "function-name";
                }
                else if (this._formLen == 2 && this.isForm(/^def/)) {
                    type = "type";
                }
                else if (this.isForm("loop") && LOOP_KEYWORDS.test(m.id)) {
                    type = "directive";
                }
            }
            m.type = type;
            this.newArg(m);
            this.token(m, type);
            return true;
        }
    }

    readString(...args) {
        if (super.readString(...args)) {
            this.newArg();
            return true;
        }
    }

    readOpenParen() {
        if (super.readOpenParen()) {
            this.newArg();
            this.pushFormStack(this._formSym);
            this.pushFormStack(this._formLen);
            this._formSym = null;
            this._formLen = 0;
            return true;
        }
    }

    readCloseParen() {
        if (super.readCloseParen()) {
            if (this._backList !== NIL) {
                this._formLen = this.popFormStack();
                this._formSym = this.popFormStack();
            }
            return true;
        }
    }

    pushFormStack(val) {
        this._formStack = new Cons(val, this._formStack);
    }
    popFormStack() {
        let val = this._formStack.car;
        this._formStack = this._formStack.cdr;
        return val;
    }

    indentation() {
        if (this._inString || this._inComment) {
            return super.indentation();
        }

        let s = this._stream;
        let INDENT_LEVEL = () => this._stream.buffer.getq("indent_level");

        var indent = 0;

        // XXX: rewrite this mess.

        var p = this._inParens.car;
        if (p) {
            var line = s.lineText(p.line);
            indent = p.col + 1;
            if (/[\#\']/.test(line.charAt(p.col - 1))) { // XXX: what's this for?
                return indent;
            }
            var nextNonSpace;
            if (this.isNameChar(line.charAt(indent))) {
                var re = /\s\S/g;
                re.lastIndex = p.col;
                nextNonSpace = re.exec(line);
                if (nextNonSpace) {
                    indent = nextNonSpace = nextNonSpace.index + 1;
                }
            }
            if (this._formLen) {
                var currentForm = this.isForm();
                if (currentForm) {
                    currentForm = currentForm.replace(/\*$/, "");
                    var formArgs = FORM_ARGS[currentForm];
                    if (!formArgs && /^with|:with/.test(currentForm)) {
                        // "with" macros usually take one argument, then &body
                        formArgs = "0*";
                    }
                    if (!formArgs && /^def/.test(currentForm)) {
                        // definitions usually take two arguments, then &body
                        if (nextNonSpace && /[\(\[\{]/.test(line.charAt(nextNonSpace)))
                            formArgs = "1*";
                        else
                            formArgs = "2*";
                    }
                    if (!formArgs) try {
                        if (LOCAL_BODYDEF[this._formStack.cdr.cdr.cdr.car.id] &&
                            this._formStack.cdr.cdr.car == 2) {
                            formArgs = "1*";
                        }
                    } catch(ex){}
                    if (formArgs) {
                        var n = parseInt(formArgs, 10);
                        var hasRest = /\+/.test(formArgs);
                        var hasBody =/\*/.test(formArgs);
                        indent = p.col + INDENT_LEVEL();
                        if (hasRest && nextNonSpace) {
                            indent = nextNonSpace;
                        }
                        else if ((n > 0 && this._formLen - 1 < n)) {
                            // still in the arguments
                            if (nextNonSpace)
                                indent = nextNonSpace;
                            else
                                indent += INDENT_LEVEL();
                        }
                    }
                }
            }
        }

        return indent;
    }
}

Ymacs_Tokenizer.define("lisp", (stream, tok, options = {}) =>
    new Ymacs_Lang_Lisp({ stream, tok, ...options }));

let Ymacs_Keymap_LispMode = Ymacs_Keymap.define("lisp", {
    '"' : [ "lisp_handle_string_quote" ], // XXX: keep this?
});

Ymacs_Buffer.newMode("lisp_mode", function() {

    var tok = this.tokenizer;
    this.setTokenizer(new Ymacs_Tokenizer({ buffer: this, type: "lisp" }));
    var changed_vars = this.setq({
        indent_level: 2,
        syntax_paragraph_sep: /\n(?:[ \t;]*\n)+/g,
        syntax_comment_line: {
            rx: /[^\S\r\n]*;+ ?/gu,
            ch: ";;"
        },
        syntax_word_dabbrev: /^[-0-9_*%+/@&$.=~\p{L}]$/u,
        paredit_space_before() {
            return !this.looking_back(/[\s\(\)\[\]\{\},.@'`#\\]/g);
        },
    });
    var was_paren_match = this.cmd("paren_match_mode", true);
    this.pushKeymap(Ymacs_Keymap_LispMode);

    var changed_commands = this.replaceCommands({
        "forward_sexp"            : "lisp_forward_sexp",
        "backward_sexp"           : "lisp_backward_sexp",
        "backward_up_list"        : "lisp_backward_up_list"
    });

    return function() {
        this.setTokenizer(tok);
        this.setq(changed_vars);
        this.newCommands(changed_commands);
        if (!was_paren_match)
            this.cmd("paren_match_mode", false);
        this.popKeymap(Ymacs_Keymap_LispMode);
    };

});
