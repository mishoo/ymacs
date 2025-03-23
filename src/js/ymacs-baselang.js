/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT

import { Cons, NIL } from "./ymacs-utils.js";
import { Ymacs_Tokenizer, compareRowCol, caretInside } from "./ymacs-tokenizer.js";
import { Ymacs_Buffer } from "./ymacs-buffer.js";

export class Ymacs_BaseLang {
    _cont = NIL;
    _inParens = NIL;
    _parens = NIL;
    _inComment = null;
    _inString = false;
    _pmeta = null;

    COMMENT = [ "//", [ "/*", "*/", "*" ] ];
    STRING = [ '"', "'" ];
    NUMBER = /^[+-]?(?:0x[0-9a-fA-F]+|(?:\d*\.)?\d+(?:[eE][+-]?\d+)?)/u;
    NAME = /^[\p{L}_$][\p{L}0-9_$]*/iu;
    WHITESPACE = /^[ \u00a0\n\r\t\f\u000b\u200b\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2007\u2008\u2009\u200a\u2028\u2029\u202f\u205f\u3000\uFEFF]+/;
    OPEN_PAREN = {
        "("  : ")",
        "{"  : "}",
        "["  : "]",
    };
    CLOSE_PAREN = {
        ")" : "(",
        "}" : "{",
        "]" : "[",
    };

    constructor({ stream, tok }) {
        this._stream = stream;
        this._tok = tok;
    }

    next() {
        this._stream.checkStop();
        if (this._cont !== NIL) {
            this._cont.car.call(this);
        } else {
            this.read();
        }
    }

    copy() {
        let _cont = this._cont;
        let _inParens = this._inParens;
        let _parens = this._parens;
        let _inComment = this._inComment;
        let _inString = this._inString;
        let _pmeta = this._pmeta;
        return () => {
            this._cont = _cont;
            this._inParens = _inParens;
            this._parens = _parens;
            this._inComment = _inComment;
            this._inString = _inString;
            this._pmeta = _pmeta;
            return this;
        };
    }

    maybeSave() {
        let s = this._stream;
        if (s.eol() && s.nextLine()) {
            this._tok.parsers[s.line] = this.copy();
            return true;
        }
    }

    skipWS(skipLines = true) {
        let s = this._stream, m;
        while (true) {
            if ((m = s.lookingAt(this.WHITESPACE))) {
                this.t(null, m[0].length);
            }
            if (skipLines) {
                if (!this.maybeSave()) break;
            } else {
                break;
            }
        }
    }

    read() {
        (this.readComment() ||
         this.readString() ||
         this.readOpenParen() ||
         this.readCloseParen() ||
         this.readCustom() ||
         this.readTrailingWhitespace() ||
         this.t());
    }

    readComment() {
        for (let syn of this.COMMENT) {
            if (Array.isArray(syn) && this.readCommentMulti(...syn)) return true;
            if (this.readCommentLine(syn)) return true;
        }
    }

    readCommentLine(start) {
        let s = this._stream;
        let p = null, end = null;
        while (s.lookingAt(start)) {
            if (!p) {
                p = {
                    line: s.line, col: s.col, c1: s.col, c2: s.col, comment: true, type: "",
                    inner: { l1: s.line, c1: s.col },
                    outer: { l1: s.line, c1: s.col },
                };
            }
            this.t("comment-starter", start.length);
            this.token({ line: s.line, c1: s.col, c2: s.col = s.lineLength() }, "comment");
            end = { line: s.line, col: s.col, c1: s.col, c2: s.col, type: "", opened: p };

            // XXX: this screws up the parse state after a line comment.
            // this.maybeSave();
            // this.skipWS(false);
        }
        if (p) {
            p.closed = end;
            p.inner.l2 = p.outer.l2 = end.line;
            p.inner.c2 = p.outer.c2 = end.c1;
            this.doneParen(p);
        }
        return p;
    }

    readCommentMulti(start, stop, inner,
                     tok1 = "mcomment-starter",
                     tok2 = "mcomment",
                     tok3 = "mcomment-stopper")
    {
        let s = this._stream, m;
        if (this._inComment) {
            if ((m = s.lookingAt(stop))) {
                let p = this.popInParen(start, m[0].length, tok3);
                p.inner.l2 = p.closed.line;
                p.inner.c2 = p.closed.c1;
                p.outer.l2 = p.closed.line;
                p.outer.c2 = p.closed.c2;
                this.popCont();
                this._inComment = null;
            } else {
                this.t(tok2);
            }
        } else if (s.lookingAt(start)) {
            this._inComment = { line: s.line, c1: s.col, inner: inner };
            let p = this.pushInParen(start, tok1);
            p.comment = true;
            p.inner = { l1: p.line, c1: p.c2 };
            p.outer = { l1: p.line, c1: p.c1 };
            this.pushCont(this.readCommentMulti.bind(this, start, stop, inner, tok1, tok2, tok3));
            return true;
        }
    }

    readString(start, stop, expStart, expStop, tokType) {
        let s = this._stream;
        if (this._inString) {
            if (s.lookingAt("\\")) {
                this.t(tokType);
                this.t(tokType);
            } else if (s.lookingAt(stop)) {
                this.popCont();
                this._inString = false;
                this.popInParen(start, stop.length, `${tokType}-stopper`);
            } else if (expStart && s.lookingAt(expStart)) {
                this._inString = false;
                let op = this.pushInParen(expStart);
                this.pushCont(() => {
                    if (s.lookingAt(expStop) && this._inParens.car === op) {
                        this.popInParen(expStart, expStop.length);
                        this.popCont();
                        this._inString = true;
                    } else {
                        this.read();
                    }
                });
            } else {
                this.t(tokType);
            }
        } else for (let syn of this.STRING) {
            let start = syn, stop = syn, expStart, expStop, tokType = "string";
            if (Array.isArray(syn)) [ start, stop, expStart, expStop, tokType = "string" ] = syn;
            if (s.lookingAt(start)) {
                this.pushInParen(start, `${tokType}-starter`);
                this._inString = true;
                this.pushCont(this.readString.bind(this, start, stop, expStart, expStop, tokType));
                return true;
            }
        }
    }

    readCustom() {
        return this.readNumber();
    }

    readName() {
        let s = this._stream, m = s.lookingAt(this.NAME);
        if (m) {
            return { line: s.line, c1: s.col, c2: s.col += m[0].length, id: m[0] };
        }
    }

    maybeName(type = null) {
        let name = this.readName();
        if (name) this.token(name, name.type = type);
        return name;
    }

    readNumber() {
        let m = this._stream.lookingAt(this.NUMBER);
        if (m) {
            this.t("number", m[0].length);
            return true;
        }
    }

    setParenMeta(info) {
        this._pmeta = info;
    }

    readOpenParen() {
        let ch = this._stream.peek();
        if (this.OPEN_PAREN[ch]) {
            this.pushInParen(ch).meta = this._pmeta;
            this._pmeta = null;
            return true;
        }
    }

    readCloseParen() {
        let type = this.CLOSE_PAREN[this._stream.peek()];
        if (type) {
            this.popInParen(type, 1);
            return true;
        }
    }

    readTrailingWhitespace() {
        let m = this._stream.lookingAt(/^\s+$/);
        if (m) {
            this.t("trailing-whitespace", m[0].length);
            return true;
        }
    }

    t(type = null, len = 1) {
        this.token({
            line: this._stream.line,
            c1: this._stream.col,
            c2: this._stream.col += len,
        }, type);
    }

    token(tok, type = tok.type) {
        this._tok.onToken(tok.line, tok.c1, tok.c2, type);
    }

    pushInParen(type, tokType = "open-paren") {
        let s = this._stream;
        let n = type.length;
        let p = { line: s.line, col: s.col, c1: s.col, c2: s.col + n, type: type };
        this._inParens = new Cons(p, this._inParens);
        if (n) this.t(tokType, n);
        return p;
    }

    popInParen(start, n = start.length, tokType = "close-paren") {
        let s = this._stream;
        if (this._inParens !== NIL) {
            let paren = this._inParens.car;
            this._inParens = this._inParens.cdr;
            if (start != paren.type) {
                //debugger;
                if (n) this.t("error", n);
            } else {
                paren.closed = { line: s.line, col: s.col, c1: s.col, c2: s.col + n, opened: paren };
                this.doneParen(paren);
                if (n) this.t(tokType, n);
            }
            return paren;
        } else {
            if (n) this.t("error", n);
        }
    }

    inParen() {
        return this._inParens.car;
    }

    doneParen(p) {
        this._parens = new Cons(p, this._parens);
    }

    pushCont(cont) {
        this._cont = new Cons(cont, this._cont);
    }

    popCont() {
        this._cont = this._cont.cdr;
    }

    // copied from JS mode; should be decent for C-like langs
    indentation() {
        let s = this._stream;
        let row = s.line;
        let currentLine = s.lineText();
        let indent = 0;

        let INDENT_LEVEL = () => this._stream.buffer.getq("indent_level");

        if (this._inString) {
            // inside string literal
            if (row > 0 && !/\S/.test(currentLine)) {
                // on an empty line, set indentation from previous line
                return s.lineIndentation(row - 1);
            }
            // otherwise keep existing indentation
            return null;
        }

        if (this._inComment) {
            let commentStartLine = s.lineText(this._inComment.line);
            indent = this._inComment.c1 + 1;
            if (this._inComment.inner == "*" && !/^\s*\*/.test(currentLine)) {
                // align with the first non-whitespace and non-asterisk character in the comment
                let re = /[^\s*]/g;
                re.lastIndex = this._inComment.c1 + 1;
                let m = re.exec(commentStartLine);
                if (m) indent = m.index;
            }
            return indent;
        }

        let p = this._inParens.car;
        if (p) {
            // check if the current line closes the paren
            let re = new RegExp("^\\s*\\" + this.OPEN_PAREN[p.type]);
            let thisLineCloses = re.test(currentLine);

            // Check if there is text after the opening paren.  If so, indent to that column.
            re = /\S/g;
            re.lastIndex = p.col + 1;
            let m = re.exec(s.lineText(p.line));
            if (m) {
                // but if this line closes the paren, better use the column of the open paren
                indent = thisLineCloses ? p.col : m.index;
            }
            else {
                // Otherwise we should indent to one level more than the indentation of the line
                // containing the opening paren. Except that if another paren is closed on that line
                // before `p`, then we'd like to use that paren's opening line instead. Oh well.
                let line = p.line;
                if (this._parens.car?.closed?.line == line) {
                    line = this._parens.car.line;
                }
                indent = s.lineIndentation(line) + INDENT_LEVEL();

                // but if this line closes the paren, then back one level
                if (thisLineCloses) {
                    indent -= INDENT_LEVEL();
                } else if (this.C_STATEMENTS && /^\s*(?:[.:?*=&|]|\+[^+]|-[^-])/.test(currentLine)) {
                    indent += INDENT_LEVEL();
                }
            }
        }
        else {
            let i = row, m;
            while (i-- > 0) if ((m = /\S/.exec(s.lineText(i)))) {
                let p = this._tok.getParserForLine(i);
                if (!(p._inString || p._inComment)) break;
            }
            if (m) indent = m.index;
        }

        // Some more adjustments for continued statements.  Since we
        // don't really have a rigorous parser, we have to rely on
        // other regexps here, which sucks but will do for now.

        if (row > 0) {
            let before = s.textBefore();
            if (/\)\s*$/.test(before) && this._parens !== NIL) {
                // Ends in a paren, could be an if, while or for which demands smart
                // indentation on the current line, let's check it out.

                // Note that the passedParen saved for that close paren is actually
                // the opening one, which suits us greatly.
                p = this._parens.car;
                let stmtLine = s.lineText(p.line);
                if (/^\s*(?:if|for|while)\W/.test(stmtLine) && !/^\s*\{/.test(currentLine))
                    indent += INDENT_LEVEL();
            }
            else if (/\Welse\s*$/.test(before) && !/^\s*\{/.test(currentLine)) {
                indent += INDENT_LEVEL();
            }
        }

        // switch labels use half the indent level, which is my favorite
        if (/^\s*(?:case|default)\W/.test(currentLine))
            indent -= INDENT_LEVEL() / 2;

        return indent;
    }

    get passedParens() {
        return [...this._parens];
    }

    get buffer() {
        return this._stream.buffer;
    }

    get caret() {
        return this.buffer._rowcol;
    }
}

Ymacs_Buffer.newCommands({
    base_limit_fill_paragraph_region: function() {
        if (!this.tokenizer) return;
        let findComment = () => this.tokenizer.getPP()
            .filter(caretInside(this._rowcol))
            .findLast(p => p.comment);
        let comment = findComment() || this.cmd("save_excursion", () => {
            this.cmd("end_of_line");
            return findComment();
        });
        if (comment?.closed) {
            let r = {
                begin: this._rowColToPosition(comment.outer.l1, comment.outer.c1),
                end: this._rowColToPosition(comment.outer.l2, comment.outer.c2)
            };
            this.cmd("save_excursion", () => {
                this.cmd("goto_char", r.begin);
                this.cmd("backward_whitespace", true);
                r.begin = this.point();
            });
            return r;
        }
    }
});
