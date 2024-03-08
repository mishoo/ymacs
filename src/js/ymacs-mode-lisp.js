//> This file is part of Ymacs, an Emacs-like editor for the Web
//> http://www.ymacs.org/
//>
//> Copyright (c) 2009-2012, Mihai Bazon, Dynarch.com.  All rights reserved.
//>
//> Redistribution and use in source and binary forms, with or without
//> modification, are permitted provided that the following conditions are
//> met:
//>
//>     * Redistributions of source code must retain the above copyright
//>       notice, this list of conditions and the following disclaimer.
//>
//>     * Redistributions in binary form must reproduce the above copyright
//>       notice, this list of conditions and the following disclaimer in
//>       the documentation and/or other materials provided with the
//>       distribution.
//>
//>     * Neither the name of Dynarch.com nor the names of its contributors
//>       may be used to endorse or promote products derived from this
//>       software without specific prior written permission.
//>
//> THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER “AS IS” AND ANY
//> EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
//> IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
//> PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER BE LIABLE
//> FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
//> CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
//> SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
//> INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
//> CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
//> ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
//> THE POSSIBILITY OF SUCH DAMAGE.

import { Ymacs_Buffer } from "./ymacs-buffer.js";
import { Ymacs_Tokenizer, Ymacs_Simple_Stream } from "./ymacs-tokenizer.js";
import { Ymacs_Keymap } from "./ymacs-keymap.js";
import { Ymacs_Interactive } from "./ymacs-interactive.js";

(function(){

    function Partial(value) {
        this.value = value;
    };

    function QuickParser(buffer, pos) {
        var input = new Ymacs_Simple_Stream({ buffer: buffer, pos: pos });
        function peek() { return input.peek() };
        function next() { return input.next() };
        function skip_ws() {
            return input.read_while(function(ch){
                if (!caret_token && caret != null && input.pos == caret) return false;
                return input.is_whitespace(ch);
            });
        };
        function skip(ch) { next() };
        function read_while(pred) { return input.read_while(pred) };
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
        };
        function read_comment() {
            return read_while(function(ch){ return ch && ch != "\n" });
        };
        function read_multiline_comment() {
            var comment = read_while(function(){
                return !input.looking_at("|#");
            });
            skip(); skip();
            return comment;
        };
        function read_string() {
            return read_escaped("\"", "\"");
        };
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
        };
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
        };
        function read_symbol() {
            return read_while(function(ch){
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
                };
                return true;
            });
        };
        function read_char() {
            return next() + read_while(function(ch){
                return (ch >= "a" && ch <= "z") ||
                    (ch >= "A" && ch <= "z") ||
                    (ch >= "0" && ch <= "9") ||
                    ch == "-" || ch == "_";
            });
        };
        function read_sharp() {
            skip("#");
            switch (peek()) {
              case "\\": next(); return token("char", read_char);
              case "/": return token("regexp", read_regexp);
              case "(": return token("vector", read_list.bind(null, "(", ")"));
              case "'": next(); return token("function", read_symbol);
              case "|": next(); return token("comment", read_multiline_comment);
              default:
                return token("unknown", read_token);
            }
        };
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
        };
        function read_all() {
            var ret = [];
            while (peek() != null) {
                var tok = read_token();
                if (tok == null) break;
                ret.push(tok);
                ++list_index;
            }
            return ret;
        };
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
                        if (tok.value === "") {
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
        };
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
            list: function() {
                var tok = cont_exp;
                while (tok && (tok.type != "list" || tok.end == caret))
                    tok = tok.parent;
                return tok;
            }
        };
    };

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
            var list = p.list();
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

    // XXX: much of the parser is actually copied from ymacs-mode-js.js.  I should somehow unify
    // the duplicate code.

    function qw(str) {
        return str.trim().split(/\s+/);
    }

    function toHash(str) {
        return qw(str).reduce((a, key, i) => (a[key] = i + 1, a), Object.create(null));
    }

    function regexp_opt(x, mods) {
        if (typeof x == "string") x = qw(x);
        return new RegExp("^(" + x.join("|") + ")$", mods);
    }

    var SPECIAL_FORMS = regexp_opt("\
define defvar defparameter defconstant deftype defstruct defclass defsetf destructuring-bind \
defmacro defun defmethod defgeneric defpackage in-package defreadtable in-readtable \
when cond unless etypecase typecase ctypecase \
lambda λ let load-time-value quote macrolet \
progn begin prog1 prog2 progv go flet the \
if throw eval-when multiple-value-prog1 multiple-value-bind unwind-protect let\\* \
ignore-errors handler-case handler-bind invoke-restart restart-case restart-bind case \
labels function symbol-macrolet block tagbody catch locally \
inc! dec! cons c[ad]{1,4}r list and or not null null\\? \
loop do while dotimes \
return return-from setq set! set-car! set-cdr! setf multiple-value-call values", "i");

    var ERROR_FORMS = toHash("error warn");

    var CONSTANTS = toHash("t nil");

    var OPEN_PAREN = {
        "(" : ")",
        "{" : "}",
        "[" : "]",
        "❰" : "❱",
        "«" : "»",
    };

    var CLOSE_PAREN = {
        ")" : "(",
        "}" : "{",
        "]" : "[",
        "❱" : "❰",
        "»" : "«",
    };

    var DEFINES_FUNCTION = toHash("defun defmacro defgeneric defmethod");

    var DEFINES_TYPE = toHash("deftype defclass defstruct");

    var FORM_ARGS = {
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

    var LOCAL_BODYDEF = toHash("labels flet macrolet");

    function isOpenParen(ch) {
        return OPEN_PAREN[ch];
    };

    function isCloseParen(ch) {
        return CLOSE_PAREN[ch];
    };

    // the tokenizer function
    Ymacs_Tokenizer.define("lisp", function(stream, tok, options){

        if (!options) options = {};

        function isConstituent(ch) {
            if (options.rx_special && options.rx_special.test(ch))
                return false;
            return ch.toLowerCase() != ch.toUpperCase() ||
                /^[-|0-9!#$%&*+./:<=>?@\^_~]$/i.test(ch);
        };

        function isConstituentStart(ch) {
            //return ch != "#" && isConstituent(ch);
            return isConstituent(ch);
        };

        var $cont          = [],
        $inString      = false,
        $inComment     = false,
        $quote         = null,
        $parens        = [],
        $passedParens  = [],
        $backList      = [],
        $list          = [],
        PARSER         = { next: next, copy: copy, indentation: indentation };

        function copy() {
            var context = restore.context = {
                cont         : $cont.slice(0),
                quote        : $quote,
                inString     : $inString,
                inComment    : $inComment,
                parens       : $parens.slice(0),
                passedParens : $passedParens.slice(0),
                backList     : $backList.slice(0),
                list         : $list.slice(0)
            };
            function restore() {
                $cont          = context.cont.slice(0);
                $inString      = context.inString;
                $quote         = context.quote;
                $inComment     = context.inComment;
                $parens        = context.parens.slice(0);
                $passedParens  = context.passedParens.slice(0);
                $backList      = context.backList.slice(0),
                $list          = context.list.slice(0);
                return PARSER;
            };
            return restore;
        };

        function foundToken(c1, c2, type) {
            tok.onToken(stream.line, c1, c2, type);
        };

        function newArg(what) {
            if (what == null)
                what = { c1: stream.col };
            $list.push(what);
        };

        function INDENT_LEVEL() { return stream.buffer.getq("indent_level"); };

        function readName() {
            var col = stream.col, ch = stream.get(),
            name = ch;
            while (!stream.eol()) {
                ch = stream.peek();
                if (!isConstituent(ch))
                    break;
                name += ch;
                stream.nextCol();
            }
            var tok = ch && { line: stream.line, c1: col, c2: stream.col, id: name.toLowerCase() };
            if (tok.c2 > tok.c1) return tok;
        };

        function readString(end, type) {
            var ch, esc = false, start = stream.col;
            while (!stream.eol()) {
                ch = stream.peek();
                if (ch === end && !esc) {
                    $cont.pop();
                    $inString = null;
                    foundToken(start, stream.col, type);
                    foundToken(stream.col, ++stream.col, type + "-stopper");
                    return true;
                }
                esc = !esc && ch === "\\";
                stream.nextCol();
            }
            foundToken(start, stream.col, type);
        };

        function readComment() {
            var line = stream.lineText(), pos = line.indexOf("|#", stream.col);
            var m = /^\s*\|+/.exec(line.substr(stream.col));
            if (m) {
                foundToken(stream.col, stream.col += m[0].length, "mcomment-starter");
            }
            if (pos >= 0) {
                $cont.pop();
                $inComment = null;
                foundToken(stream.col, pos, "mcomment");
                foundToken(pos, pos += 2, "mcomment-stopper");
                stream.col = pos;
            } else {
                foundToken(stream.col, line.length, "mcomment");
                stream.col = line.length;
            }
        };

        function isForm(form, list = $list) {
            var f = list && list.length > 0 && list[0].id;
            if (f) {
                f = f.toLowerCase();
                if (form == null)
                    return f;
                return typeof form == "string" ? f == form : f in form;
            }
        };

        function next() {
            stream.checkStop();
            if ($cont.length > 0)
                return $cont.at(-1)();
            var ch = stream.peek(), tmp;
            if ((tmp = stream.lookingAt(/^#\\.[a-z0-9_-]*/i))) {
                newArg();
                foundToken(stream.col, stream.col += tmp[0].length, "constant");
            }
            else if ((tmp = stream.lookingAt(/^#\x2f((\\.|[^\x2f])*)\x2f([igsm]*)/))) {
                newArg();
                foundToken(stream.col, stream.col += 2, "regexp-starter");
                foundToken(stream.col, stream.col += tmp[1].length, "regexp");
                foundToken(stream.col, stream.col += 1, "regexp-stopper");
                if (tmp[3])
                    foundToken(stream.col, stream.col += tmp[3].length, "regexp-modifier");
            }
            else if (stream.lookingAt(/^#\x27[^(]/)) {
                newArg();
                stream.col += 2;
                tmp = readName();
                foundToken(tmp.c1, tmp.c2, "function-name");
            }
            else if (stream.lookingAt("#|")) {
                $inComment = { line: stream.line, c1: stream.col };
                foundToken(stream.col, stream.col += 2, "mcomment-starter");
                $cont.push(readComment);
            }
            else if ((tmp = stream.lookingAt(/^;+/))) {
                foundToken(stream.col, stream.col += tmp[0].length, "comment-starter");
                foundToken(stream.col, stream.col = stream.lineLength(), "comment");
            }
            else if (ch === '"') {
                newArg();
                $inString = { line: stream.line, c1: stream.col };
                foundToken(stream.col, ++stream.col, "string-starter");
                $cont.push(readString.bind(null, ch, "string"));
            }
            else if ((tmp = stream.lookingAt(/^[+-]?(#x[0-9a-fA-F]+|#o[0-7]+|#b[01]+|[0-9]*\.?[0-9]+e?[0-9]*)(\x2f(#x[0-9a-fA-F]+|#o[0-7]+|#b[01]+|[0-9]*\.?[0-9]+e?[0-9]*))?/))) { // Dude, WTF...
                newArg();
                foundToken(stream.col, stream.col += tmp[0].length, "number");
            }
            else if ((tmp = isOpenParen(ch))) {
                newArg();
                $backList.push($list);
                $list = [];
                $parens.push({ line: stream.line, col: stream.col, type: ch });
                foundToken(stream.col, ++stream.col, "open-paren");
            }
            else if ((tmp = isCloseParen(ch))) {
                var p = $parens.pop();
                if (!p || p.type != tmp) {
                    foundToken(stream.col, ++stream.col, "error");
                } else {
                    p.closed = { line: stream.line, col: stream.col, opened: p };
                    $passedParens.push(p);
                    $list = $backList.pop();
                    foundToken(stream.col, ++stream.col, "close-paren");
                }
            }
            else if (isConstituentStart(ch) && (tmp = readName())) {
                var type = ch == ":" ? "lisp-keyword"
                    : ch == "&" ? "type"
                    : /^#:/.test(tmp.id) ? "lisp-keyword"
                    : SPECIAL_FORMS.test(tmp.id) ? "keyword"
                    : tmp.id in ERROR_FORMS ? "error"
                    : tmp.id in CONSTANTS ? "constant"
                    : null;
                if (!type) {
                    // perhaps function name?
                    if (isForm(DEFINES_FUNCTION) && $list.length == 1) {
                        type = "function-name";
                    }
                    else if (isForm(DEFINES_TYPE) && $list.length == 1) {
                        type = "type";
                    }
                    // there are a lot of macros starting with "with-", so let's highlight this
                    else if (/^with(out)?[-\x2f]|:with(out)?[-\x2f]/i.test(tmp.id)) {
                        type = "builtin";
                    }
                }
                newArg(tmp);
                foundToken(tmp.c1, tmp.c2, type);
            }
            else {
                foundToken(stream.col, ++stream.col, null);
            }
        };

        function indentation() {
            var currentLine = stream.lineText();
            if ($inString) {
                return Math.max(0, currentLine.search(/[^\s\t\n]/));
            }
            var indent = 0;

            // XXX: rewrite this mess.

            var p = $parens.at(-1);
            if (p) {
                var line = stream.lineText(p.line);
                indent = p.col + 1;
                if (/[\#\']/.test(line.charAt(p.col - 1))) {
                    return indent;
                }
                var nextNonSpace;
                if (isConstituentStart(line.charAt(indent))) {
                    var re = /\s\S/g;
                    re.lastIndex = p.col;
                    nextNonSpace = re.exec(line);
                    if (nextNonSpace) {
                        indent = nextNonSpace = nextNonSpace.index + 1;
                    }
                }
                if ($list && $list.length) {
                    var currentForm = isForm();
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
                            if (Object.hasOwn(LOCAL_BODYDEF, $backList[$backList.length - 2][0].id)) {
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
                            else if ((n > 0 && $list.length - 1 < n)) {
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
        };

        return PARSER;
    });

})();

let Ymacs_Keymap_LispMode = Ymacs_Keymap.define("lisp", {
    '"' : [ "lisp_handle_string_quote" ], // XXX: keep this?
});

Ymacs_Buffer.newMode("lisp_mode", function() {

    var tok = this.tokenizer;
    this.setTokenizer(new Ymacs_Tokenizer({ buffer: this, type: "lisp" }));
    var changed_vars = this.setq({
        indent_level: 2,
        syntax_comment_line: {
            rx: /\s*;+\s?/g,
            ch: ";;"
        },
        syntax_word_dabbrev: /^[-0-9_*%+/@&$.=~\p{L}]$/u
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
