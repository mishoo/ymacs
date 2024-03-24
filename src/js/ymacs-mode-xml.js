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

import { Ymacs_Buffer } from "./ymacs-buffer.js";
import { Ymacs_Tokenizer } from "./ymacs-tokenizer.js";
import { Ymacs_Exception } from "./ymacs-exception.js";
import { Ymacs_Keymap } from "./ymacs-keymap.js";
import { Ymacs_Interactive } from "./ymacs-interactive.js";

Ymacs_Tokenizer.define("xml", function(stream, tok) {

    var $tags = [];
    var $cont = [];
    var $parens = [];
    var $passedParens = [];
    var $inTag = null;
    var $inComment = null;
    var $inString = false;
    var PARSER = { next: next, copy: copy, indentation: indentation };

    function copy() {
        let context = resume.context = {
            tags: $tags.slice(0),
            cont: $cont.slice(0),
            parens: $parens.slice(0),
            passedParens: $passedParens.slice(0),
            inTag: $inTag,
            inComment: $inComment,
            inString: $inString,
        };
        function resume() {
            $cont = context.cont.slice(0);
            $tags = context.tags.slice(0);
            $parens = context.parens.slice(0);
            $passedParens = context.passedParens.slice(0);
            $inTag = context.inTag;
            $inComment = context.inComment;
            $inString = context.inString;
            return PARSER;
        };
        return resume;
    };

    function INDENT_LEVEL() {
        return stream.buffer.getq("indent_level");
    };

    function foundToken(c1, c2, type) {
        tok.onToken(stream.line, c1, c2, type);
    };

    function isLetter(ch) {
        return ch.toLowerCase() != ch.toUpperCase();
    };

    function isNameStart(ch) {
        return ch && (isLetter(ch) || /^[:_-]$/.test(ch));
    };

    function isNameChar(ch) {
        return ch && (isLetter(ch) || /^[0-9:_-]$/.test(ch));
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

    function readString(end) {
        $inString = true;
        var ch, esc = false, start = stream.col;
        while (!stream.eol()) {
            ch = stream.peek();
            if (!esc && ch === end) {
                $cont.pop();
                $inString = false;
                foundToken(start, stream.col, "string");
                var p = $parens.at(-1);
                if (p && p.type == ch) {
                    $parens.pop();
                    p.closed = { line: stream.line, col: stream.col, opened: p };
                    $passedParens.push(p);
                }
                foundToken(stream.col, ++stream.col, "string-stopper");
                return;
            }
            esc = !esc && ch === "\\";
            stream.nextCol();
        }
        foundToken(start, stream.col, "string");
    };

    function readTag() {
        var ch = stream.peek(), name;
        if (stream.lookingAt(/^\/>/)) {
            let p = $parens.at(-1);
            if (p && p.type == "<") {
                $parens.pop();
                p.closed = { line: stream.line, col: stream.col + 1, opened: p };
                $passedParens.push(p);
            }
            $cont.pop();
            $inTag = null;
            foundToken(stream.col, ++stream.col, "xml-closetag-slash");
            foundToken(stream.col, ++stream.col, "xml-close-bracket");
        }
        else if (ch === ">") {
            let p = $parens.at(-1);
            if (p && p.type == "<") {
                $parens.pop();
                p.closed = { line: stream.line, col: stream.col, opened: p };
                $passedParens.push(p);
            }
            $cont.pop();
            $tags.push($inTag);
            $inTag = null;
            foundToken(stream.col, ++stream.col, "xml-close-bracket");
        }
        else if (isNameStart(ch) && (name = readName())) {
            foundToken(name.c1, name.c2, "xml-attribute");
        }
        else if (ch === '"' || ch === "'") {
            $parens.push({ line: stream.line, col: stream.col, type: ch });
            foundToken(stream.col, ++stream.col, "string-starter");
            $cont.push(readString.bind(null, ch));
        }
        else foundToken(stream.col, ++stream.col, null);
    };

    function readComment(type, end) {
        var line = stream.lineText(), pos = line.indexOf(end, stream.col);
        if (pos >= 0) {
            $cont.pop();
            foundToken(stream.col, pos, type);
            $inComment = null;
            foundToken(pos, pos += end.length, type + "-stopper");
            stream.col = pos;
        } else {
            foundToken(stream.col, line.length, type);
            stream.col = line.length;
        }
    };

    function readCloseBracket() {
        var m = stream.lookingAt(/^([\s\xA0]*)(>?)/);
        if (m && m[0]) {
            if (m[1])
                foundToken(stream.col, stream.col += m[1].length, null);
            if (m[2]) {
                let p = $parens.pop();
                p.closed = { line: stream.line, col: stream.col, opened: p };
                $passedParens.push(p);
                foundToken(stream.col, stream.col += m[2].length, "xml-close-bracket");
                $cont.pop();
            }
        } else {
            foundToken(stream.col, ++stream.col, "error");
        }
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
        let ch = stream.peek(), m;
        if (stream.lookingAt("<![CDATA[")) {
            foundToken(stream.col, stream.col += 9, "xml-cdata-starter");
            $inComment = { line: stream.line, c1: stream.col };
            $cont.push(readComment.bind(null, "xml-cdata", "]]>"));
        }
        else if (stream.lookingAt("<!--")) {
            foundToken(stream.col, stream.col += 4, "mcomment-starter");
            $inComment = { line: stream.line, c1: stream.col };
            $cont.push(readComment.bind(null, "mcomment", "-->"));
        }
        else if (stream.lookingAt(/^<\//) && isNameStart(stream.peek(+2))) {
            $parens.push({ line: stream.line, c1: stream.col, c2: stream.col + 2, type: "</" });
            foundToken(stream.col, ++stream.col, "xml-open-bracket");
            foundToken(stream.col, ++stream.col, "xml-closetag-slash");
            let tag = readName(), prev = $tags.pop();
            foundToken(tag.c1, tag.c2, ( prev && prev.id == tag.id
                                         ? "xml-close-tag"
                                         : "error" ));
            if (prev) {
                let popen = { line: prev.line, c1: prev.c1, c2: prev.c2, type: prev.id };
                let pclose = { line: tag.line, c1: tag.c1, c2: tag.c2, opened: popen };
                popen.closed = pclose;
                $passedParens.push(popen);
            }
            $cont.push(readCloseBracket);
        }
        else if (ch === "<" && isNameStart(stream.peek(+1))) {
            $parens.push({ line: stream.line, col: stream.col, type: ch });
            foundToken(stream.col, ++stream.col, "xml-open-bracket");
            let tag = readName();
            foundToken(tag.c1, tag.c2, "xml-open-tag");
            $inTag = tag;
            $cont.push(readTag);
        }
        else if ((m = stream.lookingAt(/^&.*?;/))) {
            foundToken(stream.col, ++stream.col, "xml-entity-starter");
            foundToken(stream.col, stream.col += m[0].length - 2, "xml-entity");
            foundToken(stream.col, ++stream.col, "xml-entity-stopper");
        }
        else if (ch === "&") {
            foundToken(stream.col, ++stream.col, "error");
        }
        else if ((m = isOpenParen(ch))) {
            $parens.push({ line: stream.line, col: stream.col, type: ch });
            foundToken(stream.col, ++stream.col, "open-paren");
        }
        else if ((m = isCloseParen(ch))) {
            let p = $parens.pop();
            if (!p || p.type != m) {
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

    function indentation() {
        var indent, lastTag;
        if ($inComment) {
            indent = stream.lineIndentation($inComment.line) + INDENT_LEVEL();
        }
        else if ($inTag) {
            var txt = stream.lineText($inTag.line);
            if (/^\s*$/.test(txt.substr(0, $inTag.c1 - 1))) {
                indent = $inTag.c1 + $inTag.id.length + 1;
            } else {
                indent = stream.lineIndentation($inTag.line);
            }
        }
        else if ((lastTag = $tags.at(-1))) {
            indent = stream.lineIndentation(lastTag.line) + INDENT_LEVEL();
            // if current line begins with a closing tag, back one level
            if (/^\s*<\x2f/.test(stream.lineText()))
                indent -= INDENT_LEVEL();
        }
        return indent;
    };

    return PARSER;

});

let Ymacs_Keymap_XML = Ymacs_Keymap.define("xml", {
    "C-c /"   : "xml_close_tag",
    "/"       : "xml_slash_complete_tag",
    ">"       : "xml_gt_complete_tag",
    "C-Enter" : "xml_zen_expand",
    "Enter"   : "xml_newline_and_indent"
});

Ymacs_Buffer.newMode("xml_mode", function(){

    var tok = this.tokenizer;
    this.setTokenizer(new Ymacs_Tokenizer({ buffer: this, type: "xml" }));
    var was_paren_match = this.cmd("paren_match_mode", true);
    this.pushKeymap(Ymacs_Keymap_XML);
    var changed_vars = this.setq({ indent_level: 2 });
    return function() {
        this.setTokenizer(tok);
        if (!was_paren_match)
            this.cmd("paren_match_mode", false);
        this.popKeymap(Ymacs_Keymap_XML);
        this.setq(changed_vars);
    };

});

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

        xml_close_tag: Ymacs_Interactive(function() {
            this.cmd("close_last_xml_tag");
            this.cmd("indent_line");
        }),

        xml_slash_complete_tag: Ymacs_Interactive(function() {
            this.cmd("self_insert_command");
            if (this.looking_back("</")) {
                let rc = this._rowcol;
                let parser = this.tokenizer.getParserForLine(rc.row, rc.col);
                let ctx = parser.copy().context;
                let tag = ctx.tags.at(-1);
                if (tag) {
                    this.cmd("insert", tag.id, ">");
                    this.cmd("indent_line");
                }
            }
        }),

        xml_gt_complete_tag: Ymacs_Interactive(function() {
            this.cmd("self_insert_command");
            let rc = this._rowcol;
            let parser = this.tokenizer.getParserForLine(rc.row, rc.col);
            let ctx = parser.copy().context;
            let tag = ctx.tags.at(-1);
            if (tag) {
                let pos = this.point();
                this.cmd("insert", "</", tag.id, ">");
                this.cmd("goto_char", pos);
            }
        }),

        xml_zen_expand: Ymacs_Interactive(function() {
            this.cmd("xml_zen_stop");
            var html = "";
            var start = this.cmd("save_excursion", function() {
                this.cmd("backward_whitespace");
                while (!this.cmd("looking_back", /[\x20\xa0\s\t\n;&]/))
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

})();
