/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT

import { Cons, NIL } from "./ymacs-utils.js";
import { Ymacs_Tokenizer } from "./ymacs-tokenizer.js";
import { Ymacs_Buffer } from "./ymacs-buffer.js";

export class Ymacs_BaseLang {
    _cont = NIL;
    _inParens = NIL;
    _parens = NIL;
    _inComment = null;
    _inString = false;

    COMMENT = [ "//", [ "/*", "*/", "*" ] ];
    STRING = [ '"', "'" ];
    NUMBER = /^0x[0-9a-fA-F]+|^(?:\d*\.)?\d+/u;
    NAME_START = /^[_$\p{L}]/iu;
    NAME_CHAR = /^[_$\p{L}0-9]/iu;
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
        return () => {
            this._cont = _cont;
            this._inParens = _inParens;
            this._parens = _parens;
            this._inComment = _inComment;
            this._inString = _inString;
            return this;
        };
    }

    read() {
        (this.readComment() ||
         this.readString() ||
         this.readOpenParen() ||
         this.readCloseParen() ||
         this.readCustom() ||
         this.readNumber() ||
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
        if (s.lookingAt(start)) {
            this.t("comment-starter", start.length);
            this.token(s.col, s.col = s.lineLength(), "comment");
            return true;
        }
    }

    readCommentMulti(start, stop, inner) {
        let s = this._stream;
        if (this._inComment) {
            if (s.lookingAt(stop)) {
                this.popCont();
                this._inComment = null;
                this.t("mcomment-stopper", stop.length);
            } else {
                this.t("mcomment");
            }
        } else if (s.lookingAt(start)) {
            this._inComment = { line: s.line, c1: s.col, inner: inner };
            this.t("mcomment-starter", start.length);
            this.pushCont(this.readCommentMulti.bind(this, start, stop, inner));
            return true;
        }
    }

    readString(start, stop, expStart, expStop) {
        let s = this._stream;
        if (this._inString) {
            if (s.lookingAt("\\")) {
                this.t("string");
                this.t("string");
            } else if (s.lookingAt(stop)) {
                this.popCont();
                this._inString = false;
                this.popInParen(start, stop.length, "string-stopper");
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
                this.t("string");
            }
        } else for (let syn of this.STRING) {
            let start = syn, stop = syn, expStart, expStop;
            if (Array.isArray(syn)) [ start, stop, expStart, expStop ] = syn;
            if (s.lookingAt(start)) {
                this.pushInParen(start, "string-starter");
                this._inString = true;
                this.pushCont(this.readString.bind(this, start, stop, expStart, expStop));
                return true;
            }
        }
    }

    readCustom() {}

    readName() {
        let s = this._stream;
        if (s.lookingAt(this.NAME_START)) {
            let col = s.col, name = "";
            while (s.lookingAt(this.NAME_CHAR)) {
                name += s.peek();
                s.col++;
            }
            return { line: s.line, c1: col, c2: s.col, id: name };
        }
    }

    readNumber() {
        let m = this._stream.lookingAt(this.NUMBER);
        if (m) {
            this.t("number", m[0].length);
            return true;
        }
    }

    readOpenParen() {
        let ch = this._stream.peek();
        if (this.OPEN_PAREN[ch]) {
            this.pushInParen(ch);
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
        this.token(this._stream.col, this._stream.col += len, type);
    }

    token(c1, c2, type) {
        this._tok.onToken(this._stream.line, c1, c2, type);
    }

    pushInParen(type, tokType = "open-paren") {
        let s = this._stream;
        let n = type.length;
        let p = n == 1
            ? { line: s.line, col: s.col, type: type }
            : { line: s.line, c1: s.col, c2: s.col + n, type: type };
        this._inParens = new Cons(p, this._inParens);
        this.t(tokType, n);
        return p;
    }
    popInParen(start, n, tokType = "close-paren") {
        let s = this._stream;
        if (this._inParens !== NIL) {
            let paren = this._inParens.car;
            this._inParens = this._inParens.cdr;
            if (start != paren.type) {
                this.t("error", n);
            } else {
                paren.closed = n == 1
                    ? { line: s.line, col: s.col, opened: paren }
                    : { line: s.line, c1: s.col, c2: s.col + n, opened: paren };
                this.doneParen(paren);
                this.t(tokType, n);
            }
        } else {
            this.t("error", n);
        }
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
                if (thisLineCloses)
                    indent -= INDENT_LEVEL();
            }
        }
        else {
            let i = row, m;
            while (i-- > 0) if ((m = /\S/.exec(s.lineText(i)))) break;
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
                if (/^\s*(if|for|while)\W/.test(stmtLine) && !/^\s*\{/.test(currentLine))
                    indent += INDENT_LEVEL();
            }
            else if (/\Welse\s*$/.test(before) && !/^\s*\{/.test(currentLine)) {
                indent += INDENT_LEVEL();
            }
        }

        // switch labels use half the indent level, which is my favorite
        if (/^\s*(case|default)\W/.test(currentLine))
            indent -= INDENT_LEVEL() / 2;

        return indent;
    }

    get passedParens() {
        return [...this._parens];
    }
}
