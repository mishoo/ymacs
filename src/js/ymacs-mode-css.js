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

// @require ymacs-tokenizer.js

Ymacs_Tokenizer.define("css", function(stream, tok){

    var PARSER = {
        next        : next,
        copy        : copy,
        indentation : indentation
    };

    var $parens = [];
    var $passedParens = [];
    var $cont = [];
    var $inString = null;
    var $inComment = null;

    function copy() {
        var c = resume.context = {
            parens       : $parens.slice(0),
            passedParens : $passedParens.slice(0),
            cont         : $cont.slice(0),
            inString     : $inString,
            inComment    : $inComment
        };
        function resume() {
            $parens       = c.parens.slice(0);
            $passedParens = c.passedParens.slice(0);
            $cont         = c.cont.slice(0);
            $inString     = c.inString;
            $inComment    = c.inComment;
            return PARSER;
        };
        return resume;
    };

    function INDENT_LEVEL() {
        return tok.buffer.getq("indent_level");
    };

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

    function foundToken(c1, c2, type) {
        tok.onToken(stream.line, c1, c2, type);
    };

    function readComment() {
        var line = stream.lineText(), pos = line.indexOf("*/", stream.col);
        var m = /^\s*\*+/.exec(line.substr(stream.col));
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

    function next() {
        stream.checkStop();
        if ($cont.length > 0)
            return $cont.peek()();
        var ch = stream.peek(), tmp;
        if (stream.lookingAt("/*")) {
            $inComment = { line: stream.line, c1: stream.col };
            foundToken(stream.col, stream.col += 2, "mcomment-starter");
            $cont.push(readComment);
        }
        else if (ch === '"' || ch === "'") {
            $inString = { line: stream.line, c1: stream.col };
            foundToken(stream.col, ++stream.col, "string-starter");
            $cont.push(readString.$C(ch, "string"));
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
                // circular reference; poor browsers will leak.  mwuhahahaha
                p.closed = { line: stream.line, col: stream.col, opened: p };
                $passedParens.push(p);
                foundToken(stream.col, ++stream.col, "close-paren");
            }
        }
        else if ((tmp = stream.lookingAt(/^([a-zA-z-]+):/))) {
            foundToken(stream.col, stream.col += tmp[1].length, "keyword");
            foundToken(stream.col, ++stream.col, "operator");
        }
        else if ((tmp = stream.lookingAt(/^([0-9.]+)(px|pt|em|ex|in|cm|mm|%)/))) {
            foundToken(stream.col, stream.col += tmp[1].length, "number");
            foundToken(stream.col, stream.col += tmp[2].length, "type");
        }
        else if ((tmp = stream.lookingAt(/^(\.[a-zA-Z0-9_:-]+)/))) {
            foundToken(stream.col, stream.col += tmp[1].length, "function-name");
        }
        else if ((tmp = stream.lookingAt(/^(#[a-zA-Z0-9_:-]+)/))) {
            foundToken(stream.col, stream.col += tmp[1].length, "constant");
        }
        else if ((tmp = stream.lookingAt(/^(@[a-zA-Z0-9_:-]+)/))) {
            foundToken(stream.col, stream.col += tmp[1].length, "builtin");
        }
        else if ((tmp = stream.lookingAt(/^(url|none|auto|bold|italic|normal|inherit|print|screen|all)/))) {
            foundToken(stream.col, stream.col += tmp[1].length, "builtin");
        }
        else {
            foundToken(stream.col, ++stream.col, null);
        }
    };

    function indentation() {
        // no indentation for continued strings
        if ($inString)
            return 0;

        var row = stream.line;
        var currentLine = stream.lineText();
        var indent = 0;

        if ($inComment) {
            var commentStartLine = stream.lineText($inComment.line);
            indent = $inComment.c1 + 1;
            if (!/^\s*\*/.test(currentLine)) {
                // align with the first non-whitespace and non-asterisk character in the comment
                var re = /[^\s*]/g;
                re.lastIndex = $inComment.c1 + 1;
                var m = re.exec(commentStartLine);
                if (m)
                    indent = m.index;
            }
            return indent;
        }

        var p = $parens.peek();
        if (p) {
            // check if the current line closes the paren
            var re = new RegExp("^\\s*\\" + OPEN_PAREN[p.type]);
            var thisLineCloses = re.test(currentLine);

            // Check if there is text after the opening paren.  If so, indent to that column.
            var line = stream.lineText(p.line);
            re = /\S/g;
            re.lastIndex = p.col + 1;
            var m = re.exec(line);
            if (m) {
                // but if this line closes the paren, better use the column of the open paren
                indent = thisLineCloses ? p.col : m.index;
            }
            else {
                // Otherwise we should indent to one level more than the indentation of the line
                // containing the opening paren.
                indent = stream.lineIndentation(p.line) + INDENT_LEVEL();

                // but if this line closes the paren, then back one level
                if (thisLineCloses)
                    indent -= INDENT_LEVEL();
            }
        }

        return indent;
    };

    return PARSER;

});

DEFINE_SINGLETON("Ymacs_Keymap_CSS", Ymacs_Keymap);
Ymacs_Keymap_CSS().defineKeys({
    "ENTER"       : "newline_and_indent",
    ": && } && )" : "c_insert_and_indent"
});

Ymacs_Buffer.newMode("css_mode", function(){

    var tok = this.tokenizer;
    this.setTokenizer(new Ymacs_Tokenizer({ buffer: this, type: "css" }));
    var was_paren_match = this.cmd("paren_match_mode", true);
    this.pushKeymap(Ymacs_Keymap_CSS());

    return function() {
        this.setTokenizer(tok);
        if (!was_paren_match)
            this.cmd("paren_match_mode", false);
        this.popKeymap(Ymacs_Keymap_CSS());
    };

});
