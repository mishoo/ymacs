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

/* -----[ This defines the tokenizer ]----- */

(function(){

    var KEYWORDS = "abstract break case catch class const \
continue debugger default delete do else \
enum export extends final finally for \
function goto if implements import in \
instanceof interface native new package \
private protected public return static \
super switch synchronized throw \
throws transient try typeof var void let \
yield volatile while with".qw();

    var KEYWORDS_TYPE = "boolean byte char double float int long short void \
Array Date Function Math Number Object RegExp String".qw();

    var KEYWORDS_CONST = "false null undefined Infinity NaN true arguments this".qw();

    var KEYWORDS_BUILTIN = "Infinity NaN \
Packages decodeURI decodeURIComponent \
encodeURI encodeURIComponent eval isFinite isNaN parseFloat \
parseInt undefined window document alert prototype constructor".qw();

    var ALLOW_REGEXP_AFTER = /[\[({,;+\-*=?&|!:][\x20\t\n\xa0]*$|return\s+$|typeof\s+$/;

    function isLetter(ch) {
        return ch.toLowerCase() != ch.toUpperCase();
    };

    function isNameStart(ch) {
        return ch && (isLetter(ch) || /^[_$]$/.test(ch));
    };

    function isNameChar(ch) {
        return ch && (isLetter(ch) || /^[0-9_$]$/.test(ch));
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

    function JS_PARSER(KEYWORDS, KEYWORDS_TYPE, KEYWORDS_CONST, KEYWORDS_BUILTIN, stream, tok) {

        var $cont = [],
        $parens = [],
        $passedParens = [],
        $inComment = null,
        $inString = null,
        PARSER = {
            next        : next,
            copy        : copy,
            indentation : indentation
        };

        function INDENT_LEVEL() {
            return stream.buffer.getq("indent_level");
        };

        function copy() {
            var context = restore.context = {
                cont         : $cont.slice(0),
                inComment    : $inComment,
                inString     : $inString,
                parens       : $parens.slice(0),
                passedParens : $passedParens.slice(0)
            };
            function restore() {
                $cont          = context.cont.slice(0);
                $inComment     = context.inComment;
                $inString      = context.inString;
                $parens        = context.parens.slice(0);
                $passedParens  = context.passedParens.slice(0);
                return PARSER;
            };
            return restore;
        };

        function foundToken(c1, c2, type) {
            tok.onToken(stream.line, c1, c2, type);
        };

        function readName() {
            var col = stream.col, ch = stream.get(),
            name = ch;
            while (!stream.eol()) {
                ch = stream.peek();
                if (!isNameChar(ch))
                    break;
                name += ch;
                stream.nextCol();
            }
            return ch && { line: stream.line, c1: col, c2: stream.col, id: name };
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

        // function readLiteralRegexp() {
        //         var m;
        //         if (readString("/", "regexp") && (m = stream.lookingAt(/^[gmsiy]+/)))
        //                 foundToken(stream.col, stream.col += m[0].length, "regexp-modifier");
        // };

        function readLiteralRegexp() {
            var ch, esc = false, inset = 0, start = stream.col;
            while (!stream.eol()) {
                ch = stream.peek();
                if (isOpenParen(ch) && !esc && !inset)
                    inset++;
                if (isCloseParen(ch) && !esc) {
                    inset--;
                    if (inset < 0)
                        inset = 0;
                }
                if (ch === "/" && !esc && !inset) {
                    $cont.pop();
                    $inString = null;
                    foundToken(start, stream.col, "regexp");
                    foundToken(stream.col, ++stream.col, "regexp-stopper");
                    var m = stream.lookingAt(/^[gmsiy]+/);
                    if (m)
                        foundToken(stream.col, stream.col += m[0].length, "regexp-modifier");
                    return true;
                }
                esc = !esc && ch === "\\";
                stream.nextCol();
            }
            foundToken(start, stream.col, "regexp");
        };

        function next() {
            stream.checkStop();
            if ($cont.length > 0)
                return $cont.peek()();
            var ch = stream.peek(), m, tmp;
            if (stream.lookingAt("/*")) {
                $inComment = { line: stream.line, c1: stream.col };
                foundToken(stream.col, stream.col += 2, "mcomment-starter");
                $cont.push(readComment);
            }
            else if (stream.lookingAt("//")) {
                foundToken(stream.col, stream.col += 2, "comment-starter");
                foundToken(stream.col, stream.col = stream.lineLength(), "comment");
            }
            else if (ch === '"' || ch === "'") {
                $inString = { line: stream.line, c1: stream.col };
                foundToken(stream.col, ++stream.col, "string-starter");
                $cont.push(readString.$C(ch, "string"));
            }
            else if ((m = stream.lookingAt(/^0x[0-9a-f]+|^[0-9]*\.?[0-9]+/))) {
                foundToken(stream.col, stream.col += m[0].length, "number");
            }
            else if (isNameStart(ch) && (tmp = readName())) {
                var type = tmp.id in KEYWORDS ? "keyword"
                    : tmp.id in KEYWORDS_TYPE ? "type"
                    : tmp.id in KEYWORDS_CONST ? "constant"
                    : tmp.id in KEYWORDS_BUILTIN ? "builtin"
                    : null;
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
                    // circular reference; poor browsers will leak.  mwuhahahaha
                    p.closed = { line: stream.line, col: stream.col, opened: p };
                    $passedParens.push(p);
                    foundToken(stream.col, ++stream.col, "close-paren");
                }
            }
            else if (ch === "/" && ALLOW_REGEXP_AFTER.test(stream.textBefore())) {
                foundToken(stream.col, ++stream.col, "regexp-starter");
                $cont.push(readLiteralRegexp);
            }
            else if ((m = stream.lookingAt(/^\s+$/))) {
                foundToken(stream.col, stream.col += m[0].length, "trailing-whitespace");
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

            // Some more adjustments for continued statements.  Since we don't really have a
            // rigorous parser, we have to rely on other regexps here, which sucks but will do for
            // now.

            if (row > 0) {
                var before = stream.textBefore();
                if (/\)\s*$/.test(before) && $passedParens.length > 0) {
                    // Ends in a paren, could be an if, while or for which demands smart
                    // indentation on the current line, let's check it out.

                    // Note that the passedParen saved for that close paren is actually
                    // the opening one, which suits us greatly.
                    p = $passedParens.peek();
                    var stmtLine = stream.lineText(p.line);
                    if (/^\s*(if|for|while)\W/.test(stmtLine))
                        indent += INDENT_LEVEL();
                }
                else if (/\Welse\s*$/.test(before)) {
                    indent += INDENT_LEVEL();
                }
            }

            // switch labels use half the indent level, which is my favorite
            if (/^\s*(case|default)\W/.test(currentLine))
                indent -= INDENT_LEVEL() / 2;

            return indent;
        };

        return PARSER;

    };

    Ymacs_Tokenizer.define("js", JS_PARSER.$C(
        KEYWORDS.toHash(true),
        KEYWORDS_TYPE.toHash(true),
        KEYWORDS_CONST.toHash(true),
        KEYWORDS_BUILTIN.toHash(true)
    ));

    /* -----[ DynarchLIB ]----- */

    var DL_KEYWORDS_BUILTIN = KEYWORDS_BUILTIN.concat("\
DEFINE_CLASS DEFINE_SINGLETON DEFINE_HIDDEN_CLASS \
DEFAULT_ARGS DEFAULT_EVENTS \
FIXARGS CONSTRUCT BEFORE_BASE FINISH_OBJECT_DEF \
D P $".qw());

    Ymacs_Tokenizer.define("js-dynarchlib", JS_PARSER.$C(
        KEYWORDS.toHash(true),
        KEYWORDS_TYPE.toHash(true),
        KEYWORDS_CONST.toHash(true),
        DL_KEYWORDS_BUILTIN.toHash(true)
    ));

})();

/* -----[ Keymap for C-like language mode ]----- */

DEFINE_SINGLETON("Ymacs_Keymap_CLanguages", Ymacs_Keymap, function(D, P){

    D.KEYS = {
        "ENTER"                                     : "newline_and_indent",
        "} && ) && ] && : && ; && { && ( && [ && *" : "c_insert_and_indent",
        "{"                                         : "c_electric_block"
    };

});

DEFINE_SINGLETON("Ymacs_Keymap_JS", Ymacs_Keymap_CLanguages().constructor, function(D, P){});

/* -----[ Mode entry point ]----- */

Ymacs_Buffer.newMode("javascript_mode", function(useDL) {
    var tok = this.tokenizer;
    var keymap = Ymacs_Keymap_JS();
    this.setTokenizer(new Ymacs_Tokenizer({ buffer: this, type: useDL ? "js-dynarchlib" : "js" }));
    this.pushKeymap(keymap);
    var was_paren_match = this.cmd("paren_match_mode", true);
    var changed_vars = this.setq({
        syntax_comment_line: {
            rx: /\s*\x2f+\s?/g,
            ch: "//"
        }
    });

    return function() {
        this.setTokenizer(tok);
        this.popKeymap(keymap);
        if (!was_paren_match)
            this.cmd("paren_match_mode", false);
    };

});

Ymacs_Buffer.newCommands({

    javascript_dl_mode: Ymacs_Interactive(function() {
        return this.cmd("javascript_mode", true);
    }),

    c_electric_block: Ymacs_Interactive(function() {
        this.cmd("indent_line");
        this.cmd("insert", "{\n\n}");
        this.cmd("indent_line");
        this.cmd("backward_line", 1);
        this.cmd("indent_line");
    }),

    c_insert_and_indent: Ymacs_Interactive(function() {
        var ret;
        if ((ret = this.cmd("self_insert_command"))) {
            this.cmd("indent_line");
            return ret;
        }
    })

});
