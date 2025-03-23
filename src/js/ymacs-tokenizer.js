/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT

import { delayed, EventProxy } from "./ymacs-utils.js";

const TOK_DELAY = 10;

export class Ymacs_Stream {

    constructor({
        buffer = null,
        line = 0,
        col = 0
    } = {}) {
        this.buffer = buffer;
        this.line = line;
        this.col = col;
    }

    nextLine() {
        ++this.line;
        this.col = 0;
        return this.line < this.buffer.code.length;
    }

    prevLine() {
        --this.line;
        this.col = 0;
        return this.line >= 0;
    }

    peek(n = 0) {
        if (this.line < this.buffer.code.length) {
            let pos = this.col + n;
            let line = this.buffer.code[this.line];
            if (pos < line.length) return line.charAt(pos);
            if (pos == line.length) return "\n";
        }
    }

    lineText(row = this.line) {
        return this.buffer.code[row];
    }

    lineIndentation(row = this.line) {
        return /^\s*/.exec(this.lineText(row))[0].length;
    }

    lookingAt(what) {
        if (this.line < this.buffer.code.length) {
            var line = this.buffer.code[this.line];
            if (what instanceof RegExp) {
                return what.exec(line.substr(this.col));
            } else {
                return line.substr(this.col, what.length) == what ? [ what ] : null;
            }
        }
    }

    textBefore(pos) {
        if (pos == null)
            pos = this.buffer._rowColToPosition(this.line, this.col);
        return this.buffer.getCode().substr(0, pos);
    }

    textAfter(pos) {
        if (pos == null)
            pos = this.buffer._rowColToPosition(this.line, this.col);
        return this.buffer.getCode().substr(pos);
    }

    substring(start, end) {
        return this.buffer.getCode().substring(start, end);
    }

    substr(start, end) {
        return this.buffer.getCode().substr(start, end);
    }

    eol() {
        return this.col >= this.buffer.code[this.line].length;
    }

    eof() {
        var n = this.buffer.code.length, l = this.line;
        return l >= n || l == n - 1 && this.eol();
    }

    stopAt(line, col, stop = this.EOF) {
        this.__stop = { line, col, stop };
    }

    length() {
        return this.buffer.code.length;
    }

    lineLength(line) {
        if (line == null)
            line = this.line;
        return this.buffer.code[line].length;
    }

    save() {
        return { buffer: this.buffer, line: this.line, col: this.col };
    }

    restore(state) {
        this.buffer = state.buffer;
        this.line = state.line;
        this.col = state.col;
    }

    checkStop() {
        if (this.eof()) throw this.EOF;
        if (this.eol()) throw this.EOL;
        let stop = this.__stop;
        if (stop) {
            if (this.line > stop.line
                || (this.line == stop.line && this.col >= stop.col)) {
                this.__stop = null;
                throw stop.stop ?? this.EOF;
            }
        }
    }

}

Ymacs_Stream.prototype.EOL = {};
Ymacs_Stream.prototype.EOF = {};

export class Ymacs_Simple_Stream {

    constructor({
        buffer = null,
        line = 0,
        col = 0,
        pos = null
    } = {}) {
        this.buffer = buffer;
        this.line = line;
        this.col = col;
        this.pos = pos;

        if (this.pos == null) {
            this.pos = this.buffer._rowColToPosition(this.line, this.col);
        } else {
            var rc = this.buffer._positionToRowCol(this.pos);
            this.line = rc.row;
            this.col = rc.col;
        }
    }

    static is_whitespace(ch) {
        switch (ch) {
          case " ":
          case "\n":
          case "\t":
          case "\x0C":
          case "\u2028":
          case "\u2029":
          case "\xA0":
            return true;
        }
    }

    peek() {
        var a = this.buffer.code;
        var line = a[this.line];
        if (line == null) return null;
        if (this.col == line.length)
            return this.line == a.length - 1 ? null : "\n";
        return line.charAt(this.col);
    }

    next() {
        var ch = this.peek();
        if (ch) {
            ++this.pos;
            ++this.col;
            if (this.col > this.buffer.code[this.line].length) {
                this.col = 0;
                ++this.line;
            }
        }
        return ch;
    }

    read_while(pred) {
        var ret = "", ch;
        while ((ch = this.peek()) && pred(ch))
            ret += this.next();
        return ret;
    }

    skip_ws() {
        return this.read_while(this.is_whitespace);
    }

    looking_at(what) {
        var line = this.buffer.code[this.line];
        if (what instanceof RegExp) {
            return what.exec(line.substr(this.col));
        } else {
            return line.substr(this.col, what.length) == what;
        }
    }

}

Ymacs_Simple_Stream.prototype.is_whitespace = Ymacs_Simple_Stream.is_whitespace;

let LANGUAGES = Object.create(null);

export class Ymacs_Tokenizer extends EventProxy {

    static define(name, func) {
        LANGUAGES[name.toLowerCase()] = func;
    }

    constructor({ buffer, type }) {
        super();

        if (typeof type == "string") {
            type = LANGUAGES[type.toLowerCase()];
        }

        this.buffer = buffer;
        this.type = type;
    }

    reset() {
        this.stream = new Ymacs_Stream({ buffer: this.buffer });
        this.theParser = this.type(this.stream, this);
        this.parsers = [ this.theParser.copy() ];
        this.timerUpdate = null;
        this.start();
    }

    stop() {
        clearTimeout(this.timerUpdate);
    }

    getLanguage(name, options) {
        return LANGUAGES[name](this.stream, this, options);
    }

    start() {
        this.stop();
        let stream = this.stream, p, a = this.parsers;
        stream.line = a.length - 1;
        stream.col = 0;
        while (!(p = a[stream.line]))
            stream.prevLine();
        p = p();
        let doit = () => {
            this.buffer.preventUpdates();
            let n = 100;
            while (true) {
                try {
                    while (true) p.next();
                }
                catch(ex) {
                    if (ex === stream.EOL) {
                        stream.nextLine();
                        a[stream.line] = p.copy();
                        if  (--n == 0) {
                            this.buffer.resumeUpdates();
                            this.timerUpdate = setTimeout(doit, TOK_DELAY);
                            return;
                        }
                    }
                    else if (ex === stream.EOF) {
                        this.buffer.resumeUpdates();
                        break;
                    }
                    else throw ex;
                }
            }
        };
        doit();
    }

    quickInsertLine(row) {
        this.truncate(row);
    }

    quickDeleteLine(row) {
        this.truncate(row);
    }

    onToken(line, c1, c2, type) {
        this.callHooks("onFoundToken", line, c1, c2, type);
    }

    getParserForLine(row, col = 0) {
        this.stop();
        let stream = this.stream, p, a = this.parsers;
        stream.line = row;
        stream.col = 0;
        while (!(p = a[stream.line]))
            stream.prevLine();
        if (col != null) {
            stream.stopAt(row, col);
        }
        p = p();
        try {
            this.buffer.preventUpdates();
            while (true) {
                if (col == null && stream.line == row) {
                    return p;
                }
                try {
                    while (true) p.next();
                } catch(ex) {
                    if (ex === stream.EOL) {
                        stream.nextLine();
                        a[stream.line] = p.copy();
                    }
                    else if (ex === stream.EOF) {
                        return p;
                    }
                    else {
                        throw ex;
                    }
                }
            }
        } finally {
            this.buffer.resumeUpdates();
            if (stream.line < stream.length())
                this.timerUpdate = setTimeout(this.start.bind(this, stream.line), TOK_DELAY);
        }
    }

    parseUntil(row, col) {
        return this.getParserForLine(row, col);
    }

    reparseAll() {
        this.truncate(0);
        return this.finishParsing();
    }

    finishParsing() {
        this.getParserForLine(this.stream.length(), null);
        return this.getLastParser();
    }

    getLastParser() {
        return this.parsers.at(-1);
    }

    getIndentation(row, buffer) {
        var p = this.getParserForLine(row, null);
        if (p && p.indentation instanceof Function)
            return p.indentation(buffer);
    }

    truncate(row) {
        row++;
        if (this.parsers.length > row) this.parsers.length = row;
    }

    getPP() {
        let pp = this.theParser.passedParens;
        if (pp instanceof Function) pp = pp();
        return pp ? [...pp].sort(compareRowCol) : [];
    }

}

export function compareRowCol(p1, p2) {
    return ((p1.row ?? p1.line) < (p2.row ?? p2.line))
        ? -1
        : (p1.row ?? p1.line) > (p2.row ?? p2.line)
        ? 1
        : (p1.col ?? p1.c1) - (p2.col ?? p2.c1);
}

export function caretInside(caret, zone) {
    return p => (p.closed &&
                 zone == "outer" && p.outer ? (
                     compareRowCol({ line: p.outer.l1, col: p.outer.c1 }, caret) < 0
                         && compareRowCol({ line: p.outer.l2, col: p.outer.c2 }, caret) >= 0
                 )
                 : zone == "inner" && p.inner ? (
                     compareRowCol({ line: p.inner.l1, col: p.inner.c1 }, caret) <= 0
                         && compareRowCol({ line: p.inner.l2, col: p.inner.c2 }, caret) >= 0
                 )
                 : (
                     compareRowCol(p, caret) < 0
                         && compareRowCol(p.closed, caret) >= 0
                 ));
}
