// This file is part of Ymacs, an extensible source code editor
// (c) Mihai Bazon 2009 <mihai.bazon@gmail.com>
// Distributed under a BSD-style license.
// http://www.ymacs.org/

// @require ymacs-tokenizer.js

(function(){

        var SPECIAL_FORMS = "defun lambda let load-time-value quote macrolet \
progn prog1 prog2 progv go flet the \
if throw eval-when multiple-value-prog1 unwind-protect let* \
labels function symbol-macrolet block tagbody catch locally \
return-from setq multiple-value-call".qw().toHash(true);

        var COMMON_MACROS = "loop do while".qw().toHash(true);

        var CONSTANTS = "t nil".qw().toHash(true);

Ymacs_Tokenizer.define("lisp", function(stream, tok){

        var $cont          = [],
            $inString      = false,
            $inComment     = false,
            $quote         = null,
            $parens        = [],
            $passedParens  = [],
            $lastToken     = null,
            PARSER         = { next: next, copy: copy, indentation: indentation };

        function copy() {
                var context = restore.context = {
                        cont         : $cont.slice(0),
                        quote        : $quote,
                        inString     : $inString,
                        inComment    : $inComment,
                        parens       : $parens.slice(0),
                        passedParens : $passedParens.slice(0),
                        lastToken    : $lastToken
                };
                function restore() {
                        $cont          = context.cont.slice(0);
                        $inString      = context.inString;
                        $quote         = context.quote;
                        $inComment     = context.inComment;
                        $parens        = context.parens.slice(0);
                        $passedParens  = context.passedParens.slice(0);
                        $lastToken     = context.lastToken;
                        return PARSER;
                };
                return restore;
        };

        function foundToken(c1, c2, type) {
                if (type) {
                        $lastToken = {
                                c1: c1,
                                c2: c2,
                                id: stream.lineText().substring(c1, c2),
                                t: type
                        };
                }
                tok.onToken(stream.line, c1, c2, type);
        };

        function INDENT_LEVEL() { return stream.buffer.getq("indent_level"); };

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

        function next() {
                stream.checkStop();
                if ($cont.length > 0)
                        return $cont.peek()();
                var ch = stream.peek(), tmp;
                if ((tmp = stream.lookingAt(/^#\\(Space|Newline|.)/i))) {
                        foundToken(stream.col, stream.col += tmp[0].length, "constant");
                }
                else if (stream.lookingAt(/^#\x27./)) {
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
                        $inString = { line: stream.line, c1: stream.col };
                        foundToken(stream.col, ++stream.col, "string-starter");
                        $cont.push(readString.$C(ch, "string"));
                }
                else if ((tmp = stream.lookingAt(/^(#x[0-9a-f]+|#o[0-7]+|#b[01]+|[0-9]*\.?[0-9]+e?[0-9]*)(\x2f(#x[0-9a-f]+|#o[0-7]+|#b[01]+|[0-9]*\.?[0-9]+e?[0-9]*))?/))) { // Dude, WTF...
                        foundToken(stream.col, stream.col += tmp[0].length, "number");
                }
                else if (isConstituentStart(ch) && (tmp = readName())) {
                        var type = ch == ":" ? "lisp-keyword"
                                : tmp.id in SPECIAL_FORMS ? "keyword"
                                : tmp.id in COMMON_MACROS ? "builtin"
                                : tmp.id in CONSTANTS ? "constant"
                                : null;
                        if (!type) {
                                // perhaps function name?
                                if ($lastToken && $lastToken.id.toLowerCase() == "defun")
                                        type = "function-name";
                        }
                        foundToken(tmp.c1, tmp.c2, type);
                }
                else if ((tmp = isOpenParen(ch))) {
                        $parens.push({ line: stream.line, col: stream.col, type: ch });
                        foundToken(stream.col, ++stream.col, "open-paren");
                }
                else if ((tmp = isCloseParen(ch))) {
                        var p = $parens.pop();
                        if (!p || p.type != tmp) {
                                foundToken(stream.col, ++stream.col, "error");
                        } else {
                                p.closed = { line: stream.line, col: stream.col };
                                $passedParens.push(p);
                                foundToken(stream.col, ++stream.col, "close-paren");
                        }
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
                        // check if the current line closes the paren
                        var re = new RegExp("^\\s*\\" + OPEN_PAREN[p.type]);
                        var thisLineCloses = re.test(currentLine);

                        var line = stream.lineText(p.line);
                        indent = p.col + 1;
                        if (isConstituentStart(line.charAt(indent))) {
                                indent = p.col + INDENT_LEVEL();
                        }
                }

                return indent;
        };

        return PARSER;
});

Ymacs_Buffer.newMode("lisp_mode", function() {

        var tok = this.tokenizer;
        this.setTokenizer(new Ymacs_Tokenizer({ buffer: this, type: "lisp" }));
        var changed_vars = this.setq({
                indent_level: 2
        });

        return function() {
                this.setTokenizer(tok);
                this.setq(changed_vars);
        };

});

})();
