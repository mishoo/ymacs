/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT

import { Ymacs_Buffer } from "./ymacs-buffer.js";
import { Ymacs_Tokenizer } from "./ymacs-tokenizer.js";
import { Ymacs_BaseLang } from "./ymacs-baselang.js";
import { Ymacs_Keymap } from "./ymacs-keymap.js";

function toHash(a) {
    return a.reduce((a, key, i) => (a[key] = i + 1, a), Object.create(null));
}

function qw(str) {
    return toHash(str.trim().split(/\s+/));
}

const KEYWORDS = qw("abstract break case catch class const \
  async await \
  continue debugger default delete do else \
  enum export extends final finally for \
  function goto if implements import in \
  instanceof interface native new package \
  private protected public return static \
  super switch synchronized this throw \
  throws transient try typeof var void let \
  yield volatile while with");

const KEYWORDS_TYPE = qw("boolean byte char double float int long short void \
  Array Date Function Math Number Object RegExp String");

const KEYWORDS_CONST = qw("false null undefined Infinity NaN true arguments");

const KEYWORDS_BUILTIN = qw("Infinity NaN \
  Packages decodeURI decodeURIComponent \
  encodeURI encodeURIComponent eval isFinite isNaN parseFloat \
  parseInt undefined window document alert prototype constructor");

const ALLOW_REGEXP_AFTER = /[\[({,;+\-*=?&|!:][\x20\t\n\xa0]*$|(?:return|typeof|case)\s+$/;

class Ymacs_Lang_JS extends Ymacs_BaseLang {
    STRING = [ '"', "'", [ "`", "`", "${", "}" ] ];

    readCustom() {
        let s = this._stream;
        if (s.peek() == "/" && ALLOW_REGEXP_AFTER.test(s.textBefore())) {
            let op = { line: s.line, col: s.col, type: "/" };
            this.t("regexp-starter");
            this.pushCont(this.readLiteralRegexp.bind(this, op));
            return true;
        }
        let tok = this.readName();
        if (tok) {
            let type = tok.id in KEYWORDS ? "keyword"
                : tok.id in KEYWORDS_TYPE ? "type"
                : tok.id in KEYWORDS_CONST ? "constant"
                : tok.id in KEYWORDS_BUILTIN ? "builtin"
                : null;
            this.token(tok.c1, tok.c2, type);
            return true;
        }
    }

    readLiteralRegexp(op) {
        let s = this._stream;
        let ch, esc = false, inset = 0, start = s.col;
        while (!s.eol()) {
            ch = s.peek();
            if (ch == "[" && !esc && !inset) inset++;
            if (ch == "]" && !esc && inset) inset--;
            if (ch === "/" && !esc && !inset) {
                let c1 = s.col;
                this.popCont();
                this.token(start, s.col, "regexp");
                this.t("regexp-stopper");
                let m = s.lookingAt(/^[dgimsuvy]+/);
                if (m) this.t("regexp-modifier", m[0].length);
                op.closed = { line: s.line, c1: c1, c2: s.col, opened: op };
                this.doneParen(op);
                return;
            }
            esc = !esc && ch === "\\";
            s.nextCol();
        }
        this.token(start, s.col, "regexp");
    }
}

Ymacs_Tokenizer.define("js", (stream, tok) => new Ymacs_Lang_JS({ stream, tok }));

let Ymacs_Keymap_JS = Ymacs_Keymap.define("js", {
    "`"   : [ "paredit_open_pair", "`", "`", /[\`\\]/g ],
    "'"   : [ "paredit_open_pair", "'", "'", /[\'\\]/g ],
    "M-`" : [ "paredit_wrap_round", "`", "`", /[\`\\]/g ],
    "M-'" : [ "paredit_wrap_round", "'", "'", /[\'\\]/g ],
});

Ymacs_Buffer.newMode("javascript_mode", function() {
    let tok = this.tokenizer;
    this.setTokenizer(new Ymacs_Tokenizer({ buffer: this, type: "js" }));
    let was_paren_match = this.cmd("paren_match_mode", true);
    this.pushKeymap(Ymacs_Keymap_JS);
    let changed_vars = this.setq({
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
