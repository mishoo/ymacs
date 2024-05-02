/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT

import { Ymacs_Buffer } from "./ymacs-buffer.js";
import { Ymacs_Tokenizer } from "./ymacs-tokenizer.js";
import { Ymacs_BaseLang } from "./ymacs-baselang.js";

class Ymacs_Lang_CSS extends Ymacs_BaseLang {
    readCustom() {
        let s = this._stream, m;
        for (let [ rx, ...types ] of this.CSSRX) {
            if ((m = s.lookingAt(rx))) {
                types.forEach((cls, i) =>
                    cls && m[i] && this.t(cls, m[i].length));
                return true;
            }
        }
    }
    CSSRX = [
        [ /^(-?(?:\d*\.)?\d+)(px|pt|em|ex|in|cm|mm|rem|vw|vh|fr|s|%)?/u,, "number", "type" ],
        [ /^((?:--+|\$)[\p{L}\p{N}-]+)(:)?/u,, "variable-name", "operator" ],
        [ /^([\p{L}\p{N}-]+)(:)/u,, "keyword", "operator" ],
        [ /^\.[\p{L}\p{N}_:-]+/u, "function-name" ],
        [ /^#[\p{L}\p{N}_:-]+/u, "constant" ],
        [ /^@[\p{L}\p{N}_:-]+/u, "builtin" ],
        [ /^(?:url|none|auto|bold|italic|underline|normal|inherit|print|screen|all|important|calc|var)/, "builtin" ],
    ];
}

Ymacs_Tokenizer.define("css", (stream, tok) => new Ymacs_Lang_CSS({ stream, tok }));

Ymacs_Buffer.newMode("css_mode", function(){
    var tok = this.tokenizer;
    this.setTokenizer(new Ymacs_Tokenizer({ buffer: this, type: "css" }));
    var was_paren_match = this.cmd("paren_match_mode", true);
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
        this.setq(changed_vars);
    };
});
