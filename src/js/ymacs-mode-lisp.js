//> This file is part of Ymacs, an Emacs-like editor for the Web
//> http://www.ymacs.org/
//>
//> Copyright (c) 2009-2010, Mihai Bazon, Dynarch.com.  All rights reserved.
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

// @require ymacs-tokenizer.js

(function(){

        Ymacs_Buffer.newCommands({

                lisp_open_paren: Ymacs_Interactive(function(what) {
                        if (what == null)
                                what = "(";
                        what += isOpenParen(what);
                        this.cmd("insert", what);
                        this.cmd("backward_char");
                }),

                lisp_close_paren: Ymacs_Interactive(function(what) {
                        var re = new RegExp("\\s*\\" + what, "ig");
                        if (this.cmd("looking_at", re))
                                this._deleteText(this.point(), this.matchData.after);
                        this.cmd("insert", what);
                }),

                lisp_close_all_parens: Ymacs_Interactive(function() {
                        var p = this.tokenizer.getParserForLine(this._rowcol.row);
                        if (p) {
                                // this kind of sucks, we need to rewind the stream to that location..
                                var s = this.tokenizer.stream;
                                s.line = this._rowcol.row;
                                s.col = 0;
                                try {
                                        while (s.col < this._rowcol.col)
                                                p.next();
                                } catch(ex) {}
                                p = p.copy().context.parens; // these are still-to-close
                                p.r_foreach(function(p){
                                        this.cmd("lisp_close_paren", isOpenParen(p.type));
                                }, this);
                        }
                })

        });

        // XXX: much of the parser is actually copied from ymacs-mode-js.js.  I should somehow unify
        // the duplicate code.

        var SPECIAL_FORMS = "\
deftype defstruct defclass \
defmacro defun defmethod defgeneric defpackage in-package defreadtable in-readtable \
when cond unless etypecase typecase ctypecase \
lambda let load-time-value quote macrolet \
progn prog1 prog2 progv go flet the \
if throw eval-when multiple-value-prog1 unwind-protect let* \
ignore-errors handler-case case \
labels function symbol-macrolet block tagbody catch locally \
return return-from setq multiple-value-call".qw().toHash();

        var COMMON_MACROS = "loop do while".qw().toHash();

        var CONSTANTS = "t nil".qw().toHash();

        var OPEN_PAREN = {
                "(" : ")",
                "{" : "}",
                "[" : "]"
        };

        var CLOSE_PAREN = {
                ")" : "(",
                "}" : "{",
                "]" : "["
        };

        var DEFINES_FUNCTION = "defun defgeneric defmethod".qw().toHash();

        var DEFINES_TYPE = "deftype defclass defstruct".qw().toHash();

        var FORM_ARGS = {
                "if"         : "3+",
                "when"       : "1*",
                "lambda"     : "1*",
                "unless"     : "1*",
                "defun"      : "2*",
                "defgeneric" : "2*",
                "defmethod"  : "2*",
                "defclass"   : "2*",
                "defmacro"   : "2*",
                "progn"      : "0*",
                "prog1"      : "0*",
                "prog2"      : "0*",
                "let"        : "1*"
        };

        function isOpenParen(ch) {
                return OPEN_PAREN[ch];
        };

        function isCloseParen(ch) {
                return CLOSE_PAREN[ch];
        };

        function isConstituent(ch) {
                return ch.toLowerCase() != ch.toUpperCase() ||
                        /^[-0-9!#$%&*+./:<=>?@\[\]\^_\{\}~]$/i.test(ch);
        };

        function isConstituentStart(ch) {
                return ch != "#" && isConstituent(ch);
        };

        // the tokenizer function
        Ymacs_Tokenizer.define("lisp", function(stream, tok){

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
                        return ch && { line: stream.line, c1: col, c2: stream.col, id: name.toLowerCase() };
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

                function isForm(form) {
                        var f = $list && $list.length > 0 && $list[0].id;
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
                                return $cont.peek()();
                        var ch = stream.peek(), tmp;
                        if ((tmp = stream.lookingAt(/^#\\(Space|Newline|.?)/i))) {
                                newArg();
                                foundToken(stream.col, stream.col += tmp[0].length, "constant");
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
                                $cont.push(readString.$C(ch, "string"));
                        }
                        else if ((tmp = stream.lookingAt(/^[+-]?(#x[0-9a-f]+|#o[0-7]+|#b[01]+|[0-9]*\.?[0-9]+e?[0-9]*)(\x2f(#x[0-9a-f]+|#o[0-7]+|#b[01]+|[0-9]*\.?[0-9]+e?[0-9]*))?/))) { // Dude, WTF...
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
                                        : tmp.id in SPECIAL_FORMS ? "keyword"
                                        : tmp.id in COMMON_MACROS ? "builtin"
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
                                        else if (/^with-/i.test(tmp.id)) {
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
                        // no indentation for continued strings
                        if ($inString)
                                return 0;

                        var currentLine = stream.lineText();
                        var indent = 0;

                        var p = $parens.peek();
                        if (p) {
                                var line = stream.lineText(p.line);
                                indent = p.col + 1;
                                var nextNonSpace;
                                if (isConstituentStart(line.charAt(indent))) {
                                        indent = p.col + INDENT_LEVEL();
                                        var re = /\s\S/g;
                                        re.lastIndex = p.col;
                                        nextNonSpace = re.exec(line);
                                        if (nextNonSpace) {
                                                nextNonSpace = nextNonSpace.index + 1;
                                        }
                                }
                                if ($list && $list.length) {
                                        // console.log($list);
                                        var currentForm = isForm();
                                        if (currentForm) {
                                                currentForm = currentForm.replace(/\*$/, "");
                                                var formArgs = FORM_ARGS[currentForm];
                                                if (!formArgs && /^with/.test(currentForm)) {
                                                        // "with" macros usually take one argument, then &body
                                                        formArgs = "1*";
                                                }
                                                if (!formArgs) {
                                                        formArgs = "1+"; // kind of sucky now
                                                }
                                                if (formArgs) {
                                                        var n = parseInt(formArgs, 10);
                                                        var hasRest = /\+$/.test(formArgs);
                                                        var hasBody =/\*$/.test(formArgs);
                                                        // console.log("Expecting %d arguments, got %d already (rest=%o, body=%o)", n, $list.length - 1, hasRest, hasBody);
                                                        if ($list.length - 1 < n || hasRest) {
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

DEFINE_SINGLETON("Ymacs_Keymap_LispMode", Ymacs_Keymap, function(D, P){

        D.KEYS = {
                "ENTER"            : "newline_and_indent",
                "("                : [ "lisp_open_paren", "(" ],
                ")"                : [ "lisp_close_paren", ")" ],
                "C-c ] && C-c C-]" : "lisp_close_all_parens"
        };

});

Ymacs_Buffer.newMode("lisp_mode", function() {

        var tok = this.tokenizer;
        this.setTokenizer(new Ymacs_Tokenizer({ buffer: this, type: "lisp" }));
        var changed_vars = this.setq({
                indent_level: 2
        });
        var keymap = Ymacs_Keymap_LispMode();
        this.pushKeymap(keymap);
        var was_paren_match = this.cmd("paren_match_mode", true);

        return function() {
                this.setTokenizer(tok);
                this.setq(changed_vars);
                this.popKeymap(keymap);
                if (!was_paren_match)
                        this.cmd("paren_match_mode", false);
        };

});
