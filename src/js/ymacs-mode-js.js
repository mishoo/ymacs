/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT

import { Ymacs_Buffer } from "./ymacs-buffer.js";
import { Ymacs_Tokenizer } from "./ymacs-tokenizer.js";
import { Ymacs_Keymap } from "./ymacs-keymap.js";

/* -----[ This defines the tokenizer ]----- */

(function(){

    function qw(str) {
        return str.trim().split(/\s+/);
    }

    function toHash(a) {
        return a.reduce((a, key, i) => (a[key] = i + 1, a), Object.create(null));
    }

    var KEYWORDS = qw("abstract break case catch class const \
continue debugger default delete do else \
enum export extends final finally for \
function goto if implements import in \
instanceof interface native new package \
private protected public return static \
switch synchronized throw \
throws transient try typeof var void let \
yield volatile while with");

    var KEYWORDS_TYPE = qw("boolean byte char double float int long short void \
Array Date Function Math Number Object RegExp String");

    var KEYWORDS_CONST = qw("false null undefined Infinity NaN true arguments");

    var KEYWORDS_BUILTIN = qw("Infinity NaN \
Packages decodeURI decodeURIComponent \
encodeURI encodeURIComponent eval isFinite isNaN parseFloat \
parseInt undefined window document alert prototype constructor super this");

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
        "("  : ")",
        "{"  : "}",
        "["  : "]",
    };

    var CLOSE_PAREN = {
        ")" : "(",
        "}" : "{",
        "]" : "[",
    };

    function isOpenParen(ch) {
        return OPEN_PAREN[ch];
    };

    function isCloseParen(ch) {
        return CLOSE_PAREN[ch];
    };

    function JS_PARSER(KEYWORDS, KEYWORDS_TYPE, KEYWORDS_CONST, KEYWORDS_BUILTIN, stream, tok) {

        var $cont = [];
        var $parens = [];
        var $passedParens = [];
        var $inComment = null;
        var $inString = false;
        var PARSER = {
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
                passedParens : $passedParens.slice(0),
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
            var col = stream.col, ch = stream.next(),
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
            $inString = true;
            var ch, esc = false, start = stream.col;
            while (!stream.eol()) {
                ch = stream.peek();
                if (!esc && ch === end) {
                    $cont.pop();
                    $inString = false;
                    foundToken(start, stream.col, type);
                    var p = $parens.at(-1);
                    if (p && p.type == ch) {
                        $parens.pop();
                        p.closed = { line: stream.line, col: stream.col, opened: p };
                        $passedParens.push(p);
                    }
                    foundToken(stream.col, ++stream.col, type + "-stopper");
                    return;
                }
                if (!esc && end == "`" && stream.lookingAt("${")) { // start nested expression
                    $inString = false;
                    $parens.push({ line: stream.line, col: stream.col, type: "${" });
                    foundToken(start, stream.col, type);
                    foundToken(stream.col, stream.col += 2, "open-paren");
                    $cont.push(readToken);
                    return;
                }
                esc = !esc && ch === "\\";
                stream.nextCol();
            }
            foundToken(start, stream.col, type);
        };

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
                    foundToken(start, stream.col, "regexp");
                    foundToken(stream.col, ++stream.col, "regexp-stopper");
                    var m = stream.lookingAt(/^[gmsiyu]+/);
                    if (m)
                        foundToken(stream.col, stream.col += m[0].length, "regexp-modifier");
                    return true;
                }
                esc = !esc && ch === "\\";
                stream.nextCol();
            }
            foundToken(start, stream.col, "regexp");
        };

        function readToken() {
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
            else if (ch === '"' || ch === "'" || ch === "`") {
                $parens.push({ line: stream.line, col: stream.col, type: ch });
                foundToken(stream.col, ++stream.col, "string-starter");
                $cont.push(readString.bind(null, ch, "string"));
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
                if (ch == "}" && p && p.type == "${") {
                    p.closed = { line: stream.line, col: stream.col, opened: p };
                    $passedParens.push(p);
                    foundToken(stream.col, ++stream.col, "close-paren");
                    $cont.pop();
                }
                else if (!p || p.type != tmp) {
                    foundToken(stream.col, ++stream.col, "error");
                } else {
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
        }

        function next() {
            stream.checkStop();
            if ($cont.length > 0)
                return $cont.at(-1)();
            readToken();
        };

        function indentation() {
            var row = stream.line;
            var currentLine = stream.lineText();
            var indent = 0;

            if ($inString) {
                // inside string literal
                if (row > 0 && !/\S/.test(currentLine)) {
                    // on an empty line, set indentation from previous line
                    return stream.lineIndentation(row - 1);
                }
                // otherwise keep existing indentation
                return null;
            }

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

            var p = $parens.at(-1);
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
            else {
                let i = row, m;
                while (i-- > 0) if ((m = /\S/.exec(stream.lineText(i)))) break;
                if (m) indent = m.index;
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
                    p = $passedParens.at(-1);
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

    Ymacs_Tokenizer.define("js", JS_PARSER.bind(
        null,
        toHash(KEYWORDS),
        toHash(KEYWORDS_TYPE),
        toHash(KEYWORDS_CONST),
        toHash(KEYWORDS_BUILTIN)
    ));

})();

/* -----[ Keymap for C-like language mode ]----- */

let Ymacs_Keymap_JS = Ymacs_Keymap.define("js", {
    "`"   : [ "paredit_open_pair", "`", "`", /[\`\\]/g ],
    "'"   : [ "paredit_open_pair", "'", "'", /[\'\\]/g ],
    "M-`" : [ "paredit_wrap_round", "`", "`", /[\`\\]/g ],
    "M-'" : [ "paredit_wrap_round", "'", "'", /[\'\\]/g ],
});

/* -----[ Mode entry point ]----- */

Ymacs_Buffer.newMode("javascript_mode", function() {
    var tok = this.tokenizer;
    this.setTokenizer(new Ymacs_Tokenizer({ buffer: this, type: "js" }));
    var was_paren_match = this.cmd("paren_match_mode", true);
    this.pushKeymap(Ymacs_Keymap_JS);
    var changed_vars = this.setq({
        syntax_paragraph_sep: /\n(?:[ \t\/\*]*\n)+/g,
        syntax_comment_line: {
            rx: /[^\S\r\n]*\/\/+ ?/ygu,
            ch: "//"
        },
        syntax_comment_multi: {
            rx: /[^\S\r\n]*\/\*+(.*?)\*+\//ygu,
            ch: [ "/*", "*/" ]
        },
    });

    return function() {
        this.setTokenizer(tok);
        if (!was_paren_match)
            this.cmd("paren_match_mode", false);
        this.popKeymap(Ymacs_Keymap_JS);
        this.setq(changed_vars);
    };
});
