/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT

import { Ymacs_Buffer } from "./ymacs-buffer.js";
import { Ymacs_Tokenizer } from "./ymacs-tokenizer.js";

Ymacs_Tokenizer.define("markdown", function(stream, tok) {

    let PARSER = { next: next, copy: copy };
    let $inline = [];
    let $parens = [];
    let $cont = [];
    let $passedParens = [];

    function copy() {
        let context = restore.context = {
            cont         : [...$cont],
            inline       : [...$inline],
            parens       : [...$parens],
            passedParens : [...$passedParens],
        };
        function restore() {
            $cont         = [...context.cont];
            $inline       = [...context.inline];
            $parens       = [...context.parens];
            $passedParens = [...context.passedParens];
            return PARSER;
        };
        return restore;
    };

    let OPEN_PAREN = {
        "(" : ")",
        "{" : "}",
        "[" : "]",
        "«" : "»",
        "❰" : "❱",
        "“" : "”",
    };

    let CLOSE_PAREN = {
        ")" : "(",
        "}" : "{",
        "]" : "[",
        "»" : "«",
        "❱" : "❰",
        "”" : "“",
    };

    function foundToken(c1, c2, type) {
        tok.onToken(stream.line, c1, c2, type);
    };

    function isOpenParen(ch) {
        return OPEN_PAREN[ch];
    };

    function isCloseParen(ch) {
        return CLOSE_PAREN[ch];
    };

    function next() {
        stream.checkStop();
        if ($cont.length > 0)
            return $cont.at(-1)();
        readToken();
    };

    function readToken() {
        let ch = stream.peek(), tmp;
        if (stream.col == 0 && (tmp = stream.lookingAt(/^(#+)/))) {
            foundToken(0, stream.col = stream.lineLength(), "markdown-heading" + tmp[0].length);
        }
        else if (stream.line > 0 && stream.col == 0 && (tmp = stream.lookingAt(/^[=-]+$/)) && /\S/.test(stream.lineText(stream.line - 1))) {
            tmp = tmp[0].charAt(0) == "=" ? 1 : 2;
            tmp = "markdown-heading" + tmp;
            tok.onToken(stream.line - 1, 0, stream.lineLength(stream.line - 1), tmp);
            foundToken(0, stream.col = stream.lineLength(), tmp);
        }
        else if (stream.col == 0 && (tmp = stream.lookingAt(/^>\s*[>\s]*/))) {
            tmp = tmp[0].replace(/\s+/g, "").length;
            if (tmp > 3)
                tmp = "";
            tmp = "markdown-blockquote" + tmp;
            foundToken(0, stream.col = stream.lineLength(), tmp);
        }
        else if ((tmp = isOpenParen(ch))) {
            $parens.push({ line: stream.line, col: stream.col, type: ch });
            foundToken(stream.col, ++stream.col, "open-paren");
        }
        else if ((tmp = isCloseParen(ch))) {
            let p = $parens.pop();
            if (!p || p.type != tmp) {
                foundToken(stream.col, ++stream.col, "error");
            } else {
                p.closed = { line: stream.line, col: stream.col, opened: p };
                $passedParens.push(p);
                foundToken(stream.col, ++stream.col, "close-paren");
            }
        }
        else {
            foundToken(stream.col, ++stream.col, null);
        }
    };

    return PARSER;

});

Ymacs_Buffer.newMode("markdown_mode", function() {

    var tok = this.tokenizer;
    this.setTokenizer(new Ymacs_Tokenizer({ buffer: this, type: "markdown" }));
    var was_paren_match = this.cmd("paren_match_mode", true);
    return function() {
        this.setTokenizer(tok);
        if (!was_paren_match)
            this.cmd("paren_match_mode", false);
    };

});
