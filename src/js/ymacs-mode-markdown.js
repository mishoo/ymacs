/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT

import { Ymacs_Buffer } from "./ymacs-buffer.js";
import { Ymacs_Tokenizer } from "./ymacs-tokenizer.js";
import { Ymacs_BaseLang } from "./ymacs-baselang.js";
import { Ymacs_Keymap } from "./ymacs-keymap.js";

let RX_URL = /^\b((?:[a-z][\w-]+:(?:\/{1,3}|[a-z0-9%])|www\d{0,3}[.]|[a-z0-9.\-]+[.][a-z]{2,4}\/)(?:[^\s()<>]+|\(([^\s()<>]+|(\([^\s()<>]+\)))*\))+(?:\(([^\s()<>]+|(\([^\s()<>]+\)))*\)|[^\s`!()\[\]{};:'".,<>?«»“”‘’]))/i;

class Ymacs_Lang_Markdown extends Ymacs_BaseLang {
    COMMENT = [];
    STRING = [
        [ "`", "`", null, null, "markdown-code" ],
        [ "**", "**", null, null, "bold" ],
        [ "__", "__", null, null, "bold" ],
        [ "*", "*", null, null, "italic" ],
    ];
    OPEN_PAREN = {
        "(" : ")",
        "{" : "}",
        "[" : "]",
        "«" : "»",
        "❰" : "❱",
        "“" : "”",
    };
    CLOSE_PAREN = {
        ")" : "(",
        "}" : "{",
        "]" : "[",
        "»" : "«",
        "❱" : "❰",
        "”" : "“",
    };

    _block = null;

    copy() {
        let _super = super.copy();
        let _block = this._block;
        return () => {
            let self = _super();
            self._block = _block;
            return self;
        };
    }

    t(type = null, len = 1) {
        if (this._block != null) {
            type = this._block + (type != null ? " " + type : "");
        }
        this.token({
            line: this._stream.line,
            c1: this._stream.col,
            c2: this._stream.col += len,
        }, type);
    }

    next() {
        if (this._stream.eol()) this._block = null;
        return super.next();
    }

    read() {
        ((this._block != "markdown-pre" && this.readInline()) ||
         this.readCustom() ||
         this.readOpenParen() ||
         this.readCloseParen() ||
         this.readTrailingWhitespace() ||
         this.t());
    }

    readInline() {
        return this.readString();
    }

    readCustom() {
        let s = this._stream, m;
        if (s.col == 0 && (m = s.lookingAt(/^#+/))) {
            this._block = "heading" + m[0].length;
        }
        else if (s.col == 0 && (m = s.lookingAt(/^    /))) {
            this._block = "markdown-pre";
        }
        else if (s.col == 0 && (m = s.lookingAt(/^>[>\s]*/))) {
            let level = m[0].replace(/\s+/g, "").length - 1;
            level = Math.min(3, level);
            this._block = "markdown-blockquote" + (level > 0 ? level : "");
        }
        else if ((m = s.lookingAt(RX_URL))) {
            this.t(`hyperlink :href-${m[0]}`, m[0].length);
            return true;
        }
        else if ((m = s.lookingAt(/^\[[a-zA-Z0-9_-]+\]/))) {
            this.t("markdown-ref", m[0].length);
            return true;
        }
    }

    indentation() {
        let s = this._stream;
        let row = s.line;
        return row > 0 && s.lineIndentation(row - 1) || s.lineIndentation(row);
    }
}

let Ymacs_Keymap_Markdown = Ymacs_Keymap.define("markdown", {
    "`"   : [ "paredit_open_pair", "`", "`", /[\`\\]/g ],
    "M-`" : [ "paredit_wrap_round", "`", "`", /[\`\\]/g ],
});

Ymacs_Tokenizer.define("markdown", (stream, tok) => new Ymacs_Lang_Markdown({ stream, tok }));

Ymacs_Buffer.newMode("markdown_mode", function() {

    var tok = this.tokenizer;
    this.setTokenizer(new Ymacs_Tokenizer({ buffer: this, type: "markdown" }));
    var was_paren_match = this.cmd("paren_match_mode", true);
    this.pushKeymap(Ymacs_Keymap_Markdown);
    return function() {
        this.setTokenizer(tok);
        if (!was_paren_match)
            this.cmd("paren_match_mode", false);
        this.popKeymap(Ymacs_Keymap_Markdown);
    };

});
