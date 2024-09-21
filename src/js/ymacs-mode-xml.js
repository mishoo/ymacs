/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT

import { Ymacs_Buffer } from "./ymacs-buffer.js";
import { Ymacs_Tokenizer, compareRowCol, caretInside } from "./ymacs-tokenizer.js";
import { Ymacs_BaseLang } from "./ymacs-baselang.js";
import { Cons, NIL, toHash } from "./ymacs-utils.js";
import { Ymacs_Exception } from "./ymacs-exception.js";
import { Ymacs_Keymap } from "./ymacs-keymap.js";
import { Ymacs_Interactive } from "./ymacs-interactive.js";

const RX_EMPTY_TAG = /^(?:area|base|br|col|embed|hr|img|input|link|meta|param|source|track|wbr)$/i;
const RX_BLOCK_TAG = /^(?:article|aside|blockquote|body|button|caption|col|dd|div|dl|dt|fieldset|figcaption|figure|footer|form|h[1-6]|header|hgroup|li|ol|p|pre|section|table|tbody|textarea|tfoot|th|thead|td|tr|ul)$/i;

let Ymacs_Keymap_XML = Ymacs_Keymap.define("xml", {
    "C-c /"   : "xml_close_tag",
    "/"       : "xml_slash_complete_tag",
    ">"       : "xml_gt_complete_tag",
    "C-Enter" : "xml_zen_expand",
    "Enter"   : "xml_newline_and_indent"
});

let markup_mode = tok_type => function(){
    var tok = this.tokenizer;
    this.setTokenizer(new Ymacs_Tokenizer({ buffer: this, type: tok_type }));
    var was_paren_match = this.cmd("paren_match_mode", true);
    this.pushKeymap(Ymacs_Keymap_XML);
    var changed_vars = this.setq({
        indent_level: 2,
        syntax_comment_multi: {
            rx: /[^\S\r\n]*<!--+[^\S\r\n]*(.*?)[^\S\r\n]*-->/ygu,
            ch: [ "<!--", "-->" ]
        },
        syntax_word_dabbrev: /^[\p{N}_$\p{L}:#-]$/u,
        syntax_word_sexp: /^[\p{N}_$\p{L}:#-]$/u,
    });
    return function() {
        this.setTokenizer(tok);
        if (!was_paren_match)
            this.cmd("paren_match_mode", false);
        this.popKeymap(Ymacs_Keymap_XML);
        this.setq(changed_vars);
    };
};

Ymacs_Buffer.newMode("xml_mode", markup_mode("xml"));
Ymacs_Buffer.newMode("html_mode", markup_mode("html"));
Ymacs_Buffer.newMode("twig_html_mode", markup_mode("twig_html"));

class Ymacs_Lang_XML extends Ymacs_BaseLang {
    _tags = NIL;
    _inTag = null;
    _inline = 0;

    COMMENT = [
        [ "<!--", "-->", "-" ],
        [ "<![CDATA[", "]]>", null,
          "xml-cdata-starter", "xml-cdata", "xml-cdata-stopper" ]
    ];
    NAME = /^[\p{L}:_$-][\p{L}0-9:_$-]*/iu;
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

    constructor({ stream, tok, emptyTags, inline }) {
        super({ stream, tok });
        this.emptyTags = emptyTags;
        this.inline = inline;
    }

    copy() {
        let _super = super.copy();
        let _tags = this._tags;
        let _inTag = this._inTag;
        let _inline = this._inline;
        return () => {
            let self = _super();
            self._tags = _tags;
            self._inTag = _inTag;
            self._inline = _inline;
            return self;
        };
    }

    read() {
        (this.readComment() ||
         this.readDeclaration() ||
         this.readCloseTag() ||
         this.readOpenTag() ||
         this.readOpenParen() ||
         this.readCloseParen() ||
         this.readTrailingWhitespace() ||
         this.t(null, 1, this._stream.peek() != " "));
    }

    t(type = null, len = 1, addInline) {
        if (addInline && this.inline) {
            type = this.inline.cls(this._inline) + " " + type;
        }
        this.token(this._stream.col, this._stream.col += len, type);
    }

    readDeclaration() {
        let s = this._stream, m = s.lookingAt(/^<[?!]/);
        if (m) {
            let sp = m[0];
            this.pushInParen(sp, "xml-open-bracket");
            this.pushCont(() => {
                let m = s.lookingAt(/^\??>/);
                if (m) {
                    this.popInParen(sp, m[0].length, "xml-close-bracket");
                    this.popCont();
                } else {
                    this.readString() || this.readTrailingWhitespace() || this.t("directive");
                }
            });
            return true;
        }
    }

    readOpenTag() {
        let s = this._stream;
        if (s.lookingAt("<") && this.NAME.test(s.peek(+1))) {
            let outer = { l1: s.line, c1: s.col };
            this.pushInParen("<", "xml-open-bracket");
            let tag = this.readName();
            tag.outer = outer;
            this.token(tag.c1, tag.c2, "xml-open-tag");
            this._inTag = tag;
            this.pushCont(this.contOpenTag);
            return true;
        }
    }

    contOpenTag() {
        let s = this._stream;
        let ch = s.peek(), name;
        if (s.lookingAt("/>")) {
            this.popInParen("<", 2, "xml-close-bracket");
            this.popCont();
            this._inTag = null;
        }
        else if (ch === ">") {
            this.popInParen("<", 1, "xml-close-bracket");
            this.popCont();
            if (!this.emptyTags?.test(this._inTag.id)) {
                this._inTag.inner = { l1: s.line, c1: s.col };
                this.pushTag(this._inTag);
                if (this.inline) {
                    this._inline ^= this.inline.code(this._inTag.id);
                }
            }
            this._inTag = null;
        }
        else if ((name = this.readName())) {
            this.token(name.c1, name.c2, "xml-attribute");
        }
        else if (ch === '"' || ch === "'") {
            this.readString();
        }
        else {
            this.readTrailingWhitespace() || this.t();
        }
    }

    readCloseTag() {
        let s = this._stream;
        if (s.lookingAt("</") && this.NAME.test(s.peek(+2))) {
            let otag = this.popTag();
            if (otag) {
                otag.inner.l2 = s.line;
                otag.inner.c2 = s.col;
                if (this.inline) {
                    this._inline ^= this.inline.code(otag.id);
                }
            }
            this.pushInParen("</", "xml-open-bracket");
            let ctag = this.readName();
            this.token(ctag.c1, ctag.c2, otag?.id == ctag.id ? "xml-close-tag" : "error");
            if (otag) {
                let popen = { line: otag.line, c1: otag.c1, c2: otag.c2, type: otag.id,
                              inner: otag.inner, outer: otag.outer };
                let pclose = { line: ctag.line, c1: ctag.c1, c2: ctag.c2, opened: popen };
                popen.closed = pclose;
                this.doneParen(popen);
            }
            this.pushCont(this.contCloseTag.bind(this, otag));
            return true;
        }
    }

    contCloseTag(otag) {
        let s = this._stream, m = s.lookingAt(/^([\s\xA0]*)(>?)/);
        if (m && m[0]) {
            if (m[1]) this.t(null, m[1].length);
            if (m[2]) {
                this.popInParen("</", 1, "xml-close-bracket");
                this.popCont();
                if (otag) {
                    otag.outer.l2 = s.line;
                    otag.outer.c2 = s.col;
                }
            }
        } else {
            this.readTrailingWhitespace() || this.t("error");
        }
    }

    indentation() {
        let s = this._stream;
        var indent, lastTag;

        let INDENT_LEVEL = () => this._stream.buffer.getq("indent_level");

        if (this._inComment) {
            indent = s.lineIndentation(this._inComment.line) + INDENT_LEVEL();
        }
        else if (this._inTag) {
            var txt = s.lineText(this._inTag.line);
            if (/^\s*$/.test(txt.substr(0, this._inTag.c1 - 1))) {
                indent = this._inTag.c1 + this._inTag.id.length + 1;
            } else {
                indent = s.lineIndentation(this._inTag.line);
            }
        }
        else if ((lastTag = this._tags.car)) {
            indent = s.lineIndentation(lastTag.line) + INDENT_LEVEL();
            // if current line begins with a closing tag, back one level
            if (/^\s*<\x2f/.test(s.lineText()))
                indent -= INDENT_LEVEL();
        }

        if (indent == null) {
            let line = s.line;
            while (line > 0 && !/\S/.test(s.lineText(line))) line--;
            indent = s.lineIndentation(line);
        }

        return indent;
    }

    pushTag(tag) {
        this._tags = new Cons(tag, this._tags);
    }
    popTag() {
        let tag = this._tags.car;
        this._tags = this._tags.cdr;
        return tag;
    }
    get tag() {
        return this._tags.car;
    }
}

class Ymacs_Lang_HTML extends Ymacs_Lang_XML {
    _mode = this;
    _js = this._tok.getLanguage("js");
    _css = this._tok.getLanguage("css");

    get passedParens() {
        return [
            ...super.passedParens,
            ...this._js.passedParens,
            ...this._css.passedParens,
        ];
    }

    next() {
        let s = this._stream;
        if (this._mode === this && this.tag) {
            if (this.tag.id == "script") this._mode = this._js;
            if (this.tag.id == "style") this._mode = this._css;
        }
        if (this._mode === this._js && s.lookingAt("</script")) {
            this._mode = this;
        }
        if (this._mode === this._css && s.lookingAt("</style")) {
            this._mode = this;
        }
        this._mode === this ? super.next() : this._mode.next();
    }

    copy() {
        let _super = super.copy();
        let _js = this._js.copy();
        let _css = this._css.copy();
        let _mode = this._mode;
        return () => {
            let self = _super();
            self._js =_js();
            self._css = _css();
            self._mode = _mode;
            return self;
        };
    }

    indentation() {
        if ((this._mode === this) || (!this._mode._inString && /^\s*<\//.test(this._stream.lineText()))) {
            return super.indentation();
        } else {
            return this._mode.indentation();
        }
    }
}

const TWIG_BUILTIN = toHash("in or and not is defined \
  constant divisible empty even iterable \
  null true false odd same from as \
  starts with only ends matches");

class Ymacs_Lang_Twig extends Ymacs_BaseLang {
    STRING = [ "'", [ '"', '"', "#{", "}" ] ];
    _blocks = NIL;
    _inBlock = null;
    _alt = this._tok.getLanguage("html");
    _mode = this._alt;

    get passedParens() {
        return [
            ...super.passedParens,
            ...this._alt.passedParens,
        ];
    }

    copy() {
        let _super = super.copy();
        let _alt = this._alt.copy();
        let _mode = this._mode;
        let _blocks = this._blocks;
        let _inBlock = this._inBlock;
        return () => {
            let self = _super();
            self._alt =_alt();
            self._mode = _mode;
            self._blocks = _blocks;
            self._inBlock = _inBlock;
            return self;
        };
    }

    next() {
        let s = this._stream, m;
        if (this._mode === this._alt) {
            if ((m = s.lookingAt(/^\{%-?/))) {
                this._mode = this;
                this.pushCont(this.readBlock.bind(this, m[0]));
            } else if ((m = s.lookingAt(/^\{\{-?/))) {
                this._mode = this;
                this.pushInParen(m[0], "exp-starter");
            } else if ((m = s.lookingAt(/^\{#-?/))) {
                this._alt.readCommentMulti(m[0], /^-?#\}/, "#");
                return this.next();
            }
        } else if ((m = s.lookingAt(/^-?\}\}/)) && /^\{\{/.test(this._inParens.car?.type)) {
            this.popInParen(this._inParens.car?.type, m[0].length, "exp-stopper");
            this._mode = this._alt;
            return this.next();
        }
        this._mode === this ? super.next() : this._alt.next();
    }

    readCustom() {
        let tok = this.readName();
        if (tok) {
            let type = tok.id in TWIG_BUILTIN ? "builtin" : null;
            this.token(tok.c1, tok.c2, type);
            return true;
        }
        return this.readNumber();
    }

    readBlock(start) {
        let s = this._stream, m;
        if (this._inBlock) {
            this.skipWS();
            if ((m = s.lookingAt(/^-?%\}/))) {
                this.popInParen(start, m[0].length, "block-stopper");
                this._mode = this._alt;
                this.popCont();
                if (this._inBlock.hasBody) {
                    this._inBlock.inner = { l1: s.line, c1: s.col };
                    this.pushBlock(this._inBlock);
                }
                this._inBlock = null;
            } else {
                this.read();
            }
        } else {
            let outer = { l1: s.line, c1: s.col };
            this.pushInParen(start, "block-starter");
            this.skipWS();
            let ctag = this.readName();
            if (ctag) {
                let isEndTag = /^end/.test(ctag.id);
                if (isEndTag) {
                    let otag = this.popBlock();
                    let name = ctag.id.substr(3);
                    this.token(ctag.c1, ctag.c2, otag?.id == name ? "keyword" : "error");
                    if (otag) {
                        let popen = { line: otag.line, c1: otag.c1, c2: otag.c2, type: otag.id,
                                      inner: otag.inner, outer: otag.outer };
                        let pclose = { line: ctag.line, c1: ctag.c1, c2: ctag.c2, opened: popen };
                        popen.closed = pclose;
                        this.doneParen(popen);
                        this._inBlock = otag; // waiting for %}
                        delete otag.hasBody; // no more body to parse
                    }
                    this.skipWS();
                    if (name == "macro") {
                        let fname = this.readName();
                        if (fname) {
                            this.token(fname.c1, fname.c2, fname.id == otag.fname?.id ? "function-name" : "error");
                        }
                        this.skipWS();
                    }
                } else {
                    this.token(ctag.c1, ctag.c2, "keyword");
                    ctag.outer = outer;
                    this._inBlock = ctag;
                    this.littleParseTag(ctag);
                }
            }
        }
    }

    littleParseTag(ctag) {
        let s = this._stream;
        ctag.hasBody = !/^(import|do|from|include|extends|use|else(?:if)?)$/.test(ctag.id);
        this.skipWS();
        if (ctag.id == "set") {
            while (this.maybeName("variable-name")) {
                this.skipWS();
                if (s.lookingAt(",")) {
                    s.col++;
                    this.skipWS();
                } else break;
            }
            this.skipWS();
            ctag.hasBody = s.lookingAt(/^-?%\}/);
        } else if (ctag.id == "block" || ctag.id == "macro") {
            ctag.fname = this.maybeName("function-name");
        }
    }

    maybeName(type = null) {
        let name = this.readName();
        if (name) {
            this.token(name.c1, name.c2, type);
        }
        return name;
    }

    indentation() {
        let s = this._stream;
        let INDENT_LEVEL = () => this._stream.buffer.getq("indent_level");
        if (this._mode === this) {
            return super.indentation();
        } else if (this.block) {
            let indent = s.lineIndentation(this.block.line);
            let closing = /^\s*\{%-?\s*end/.test(s.lineText());
            if (closing) return indent;
            if (!this._mode.tag || this._mode.tag.line <= this.block.line)
                return indent + INDENT_LEVEL();
        }
        return this._mode.indentation();
    }

    pushBlock(block) {
        this._blocks = new Cons(block, this._blocks);
    }
    popBlock() {
        let block = this._blocks.car;
        this._blocks = this._blocks.cdr;
        return block;
    }
    get block() {
        return this._blocks.car;
    }
}

function inlineCode(tag) {
    return (tag == "i" || tag == "em" ? 1 :
            tag == "b" || tag == "strong" ? 2 :
            tag == "a" ? 4 :
            tag == "h1" ? 8 :
            tag == "h2" ? 16 :
            tag == "h3" ? 32 :
            tag == "h4" ? 64 :
            0);
}

function inlineCls(inline) {
    let out = "";
    if (inline & 1) out += " italic";
    if (inline & 2) out += " bold";
    if (inline & 4) out += " link";
    if (inline & 8) out += " heading1";
    if (inline & 16) out += " heading2";
    if (inline & 32) out += " heading3";
    if (inline & 64) out += " heading4";
    return out.trim() || null;
}

Ymacs_Tokenizer.define("xml", (stream, tok) => new Ymacs_Lang_XML({ stream, tok }));

Ymacs_Tokenizer.define("html", (stream, tok) => new Ymacs_Lang_HTML({
    stream, tok,
    emptyTags: RX_EMPTY_TAG,
    inline: { code: inlineCode, cls: inlineCls },
}));

Ymacs_Tokenizer.define("twig_html", (stream, tok) => new Ymacs_Lang_Twig({ stream, tok }));

Ymacs_Buffer.newCommands({
    xml_get_fill_paragraph_region: function() {
        // XXX: this function should somehow be a property of the language (for mixed modes),
        // rather than a top-level method. Not yet decided how to best handle this...
        if (!this.tokenizer) return;

        this.tokenizer.finishParsing();
        let blk = this.tokenizer.getPP()
            .filter(caretInside(this._rowcol, "outer"))
            .findLast(p => RX_BLOCK_TAG.test(p.type));
        if (blk) {
            let r = {
                begin: this._rowColToPosition(blk.inner.l1, blk.inner.c1),
                end: this._rowColToPosition(blk.inner.l2, blk.inner.c2)
            };
            this.cmd("save_excursion", () => {
                this.cmd("goto_char", r.begin);
                if (this.cmd("looking_at", /[^\S\r\n]*\n/gy)) {
                    this.cmd("goto_char", this.matchData.after);
                    r.begin = this.point();
                }
                this.cmd("goto_char", r.end);
                if (this.cmd("looking_back", /\n[^\S\r\n]*/g)) {
                    this.cmd("backward_whitespace");
                    r.end = this.point();
                }
            });
            return r;
        }
    },
    xml_close_tag: Ymacs_Interactive(function() {
        this.cmd("close_last_xml_tag");
        this.cmd("indent_line");
    }),
    xml_slash_complete_tag: Ymacs_Interactive(function() {
        this.cmd("self_insert_command");
        if (this.looking_back("</")) {
            let rc = this._rowcol;
            this.tokenizer.parseUntil(rc.row, rc.col);
            let tag = this.tokenizer.theParser.tag;
            if (tag) {
                this._placeUndoBoundary();
                this.cmd("insert", tag.id, ">");
                this.cmd("indent_line");
            }
        }
    }),
    xml_gt_complete_tag: Ymacs_Interactive(function() {
        this.cmd("self_insert_command");
        let rc = this._rowcol;
        this.tokenizer.parseUntil(rc.row, rc.col);
        let tag = this.tokenizer.theParser.tag;
        if (tag?.inner?.l1 == rc.row && tag.inner.c1 == rc.col) {
            this._placeUndoBoundary();
            let pos = this.point();
            this.cmd("insert", "</", tag.id, ">");
            this.cmd("goto_char", pos);
        }
    }),
    xml_newline_and_indent: Ymacs_Interactive(function(){
        let inparens = this.looking_at("</") && this.looking_back(">");
        this.cmd("newline_and_indent");
        if (inparens) {
            this.cmd("newline_and_indent");
            this.cmd("backward_line");
            this.cmd("indent_line");
        }
    }),
});

// not sure "zen mode" is of any interest these days, but let's leave it for now.
(function(){

    let Ymacs_Keymap_XML_Zen = Ymacs_Keymap.define("xml_zen", {
        "Tab"   : "xml_zen_next_poi",
        "S-Tab" : "xml_zen_prev_poi",
        "C-g"   : "xml_zen_stop"
    });

    var MODE_TYPE = 1, MODE_CLASS = 2, MODE_ID = 3, MODE_REPEAT = 4, MODE_ATTR = 5;

    function zen_render(el, html) {
        var n = el.repeat || 1;
        for (var i = 1; i <= n; ++i) {
            if (i > 1)
                html("\n");
            html("<", el.type);
            if (el.id) {
                html(' id="', el.id.replace(/\$/g, i), '"');
            }
            if (el.klass) {
                html(' class="', el.klass.replace(/\$/g, i), '"');
            }
            if (el.attributes) {
                el.attributes.forEach(attr => html(" ", attr, '="|"'));
            }
            html(">");
            if (el.child) {
                html("\n");
                zen_render(el.child, html);
                html("\n");
            } else {
                html("|");
            }
            html("</", el.type, ">");
            if (el.next) {
                html("\n");
                zen_render(el.next, html);
            }
        }
    };

    function zen_parse(str, i) {
        var el = { type: "" }, mode = MODE_TYPE;
        OUTER: while (i < str.length) {
            var ch = str.charAt(i++);
            switch (ch) {

              case "#":
                mode = MODE_ID;
                el.id = "";
                break;

              case ".":
                mode = MODE_CLASS;
                if (el.klass != null) {
                    el.klass += " ";
                } else {
                    el.klass = "";
                }
                break;

              case ":":
                mode = MODE_ATTR;
                if (el.attributes == null)
                    el.attributes = [];
                el.attributes.push("");
                break;

              case "*":
                mode = MODE_REPEAT;
                el.repeat = "";
                break;

              case ">":
                el.child = zen_parse(str, i);
                i = el.child.i;
                break OUTER;

              case "(":
                el.child = zen_parse(str, i);
                i = el.child.i;
                break;

              case ")":
                break OUTER;

              case "+":
                el.next = zen_parse(str, i);
                i = el.next.i;
                break OUTER;

              default:
                switch (mode) {
                  case MODE_TYPE:
                    el.type += ch;
                    break;
                  case MODE_CLASS:
                    el.klass += ch;
                    break;
                  case MODE_ID:
                    el.id += ch;
                    break;
                  case MODE_REPEAT:
                    el.repeat = parseInt(String(el.repeat) + ch, 10);
                    break;
                  case MODE_ATTR:
                    el.attributes.push(el.attributes.pop() + ch);
                    break;
                }
            }
        }

        el.i = i;
        return el;
    };

    function maybe_stop_zen() {
        var point = this.point(),
        a = this.getq("xml_zen_markers"),
        start = a[0],
        end = a.at(-1);
        if (point < start.getPosition() || point > end.getPosition() ||
            end.getPosition() == a.at(-2).getPosition()) {
            this.cmd("xml_zen_stop");
        }
    };

    Ymacs_Buffer.newCommands({

        xml_zen_expand: Ymacs_Interactive(function() {
            this.cmd("xml_zen_stop");
            var html = "";
            var start = this.cmd("save_excursion", function() {
                this.cmd("backward_whitespace");
                while (!this.cmd("looking_back", /[\x20\xa0\s\t\n;&]/g))
                    if (!this.cmd("backward_char"))
                        break;
                return this.point();
            });
            var point = this.point();

            try {
                zen_render(
                    zen_parse(
                        this.cmd("buffer_substring", start, point).trim(), 0
                    ),
                    (...args) => html += args.join("")
                );
            } catch(ex) {
                throw new Ymacs_Exception("The Zen is not strong today :-/");
            }

            this.cmd("delete_region", start, point);
            this.cmd("insert", html);
            start = this.createMarker(start, false, "xml_zen");

            // locate points of interest
            var end = this.createMarker(this.point(), true, "xml_zen"), markers = [];
            this.cmd("goto_char", start.getPosition());
            while (this.cmd("search_forward", "|", end.getPosition())) {
                this.cmd("backward_delete_char");
                markers.push(this.createMarker(this.point(), true, "xml_zen_start"));
                markers.push(this.createMarker(this.point(), false, "xml_zen_end"));
            }

            this.cmd("indent_region", start.getPosition(), end.getPosition());

            var count = markers.length;
            if (count > 0) {
                // move to first POI
                this.cmd("goto_char", markers[0]);
                markers.unshift(start);
                markers.push(end);
                this.setq("xml_zen_markers", markers);
                this.pushKeymap(Ymacs_Keymap_XML_Zen);
                this.addEventListener("afterInteractiveCommand", maybe_stop_zen);
            } else {
                start.destroy();
                end.destroy();
            }
        }),

        xml_zen_stop: Ymacs_Interactive(function(){
            var tmp = this.getq("xml_zen_markers");
            if (tmp) {
                tmp.map(m => m.destroy());
                this.setq("xml_zen_markers", null);
            }
            this.popKeymap(Ymacs_Keymap_XML_Zen);
            this.removeEventListener("afterInteractiveCommand", maybe_stop_zen);
        }),

        xml_zen_next_poi: Ymacs_Interactive(function(){
            let markers = this.getq("xml_zen_markers"), pos = this.point();
            for (let i = 0; i < markers.length; ++i) {
                let m = markers[i];
                if (m.getPosition() > pos) {
                    this.cmd("goto_char", m.getPosition());
                    break;
                }
            }
        }),

        xml_zen_prev_poi: Ymacs_Interactive(function(){
            let markers = this.getq("xml_zen_markers"), pos = this.point();
            for (let i = markers.length; --i >= 0;) {
                let m = markers[i];
                if (m.getPosition() < pos) {
                    this.cmd("goto_char", m.getPosition());
                    break;
                }
            }
        }),

    });

})();
