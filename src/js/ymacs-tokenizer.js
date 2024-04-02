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

import { delayed, EventProxy } from "./ymacs-utils.js";

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

    nextCol() {
        ++this.col;
    }

    prevCol() {
        --this.col;
    }

    nextLine() {
        ++this.line;
        this.col = 0;
    }

    prevLine() {
        --this.line;
        this.col = 0;
    }

    peek(n = 0) {
        if (this.line < this.buffer.code.length) {
            let pos = this.col + n;
            let line = this.buffer.code[this.line];
            if (pos < line.length) return line.charAt(pos);
            if (pos == line.length) return "\n";
        }
    }

    next() {
        let ch = this.peek();
        this.nextCol();
        if (this.col > this.buffer.code[this.line].length)
            this.nextLine();
        let stop = this.__stop;
        if (stop) {
            if (this.line > stop.line
                || (this.line == stop.line && this.col >= stop.col)) {
                this.__stop = null;
                throw stop.stop ?? this.EOF;
            }
        }
        return ch;
    }

    lineText(row) {
        if (row == null)
            row = this.line;
        return this.buffer.code[row];
    }

    lineIndentation(row) {
        return /^\s*/.exec(this.lineText(row))[0].length;
    }

    lookingAt(what) {
        var line = this.buffer.code[this.line];
        if (what instanceof RegExp) {
            return what.exec(line.substr(this.col));
        } else {
            return line.substr(this.col, what.length) == what;
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
        return this.col == this.buffer.code[this.line].length;
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

    skip_ws() {
        while (Ymacs_Simple_Stream.is_whitespace(this.peek()))
            this.next();
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
        this.parsers = [];
        this.parsers[-1] = this.theParser.copy();
        this.timerUpdate = null;
        this.start();
    }

    stop() {
        clearTimeout(this.timerUpdate);
    }

    getLanguage(name, options) {
        return LANGUAGES[name](this.stream, this, options);
    }

    showProgress(p) {
        if (p != null) {
            p = Math.round(p / this.stream.length() * 100) + "%";
        }
        this.buffer.updateProgress("Syntax highlighting", p);
    }

    start() {
        this.stop();
        let stream = this.stream, p, a = this.parsers;
        stream.line = a.length;
        while (!(p = a[stream.line]))
            stream.prevLine();
        stream.nextLine();
        p = p();
        let doit = () => {
            this.buffer.preventUpdates();
            let n = 100;
            // this.showProgress(this.stream.line);
            while (true) {
                try {
                    while (true) p.next();
                }
                catch(ex) {
                    if (ex === stream.EOL) {
                        a[stream.line] = p.copy();
                        stream.nextLine();
                        if  (--n == 0) {
                            this.buffer.resumeUpdates();
                            this.timerUpdate = setTimeout(doit, 25);
                            return;
                        }
                    }
                    else if (ex === stream.EOF) {
                        a[stream.line] = p.copy();
                        this.buffer.resumeUpdates();
                        break;
                    }
                    else throw ex;
                }
            }
            // this.showProgress();
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

    getParserForLine(row, col) {
        this.stop();
        let stream = this.stream, p, a = this.parsers;
        stream.line = row - 1;
        while (!(p = a[stream.line]))
            stream.prevLine();
        stream.nextLine();
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
                        a[stream.line] = p.copy();
                        stream.nextLine();
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
                this.timerUpdate = setTimeout(this.start.bind(this, stream.line), 25);
        }
    }

    reparseAll() {
        this.truncate(0);
        return this.finishParsing();
    }

    finishParsing() {
        this.getParserForLine(this.stream.length());
        return this.getLastParser();
    }

    getLastParser() {
        return this.parsers.at(-1);
    }

    getIndentation(row, buffer) {
        var p = this.getParserForLine(row);
        if (p && p.indentation instanceof Function)
            return p.indentation(buffer);
    }

    truncate(row) {
        if (this.parsers.length > row) this.parsers.length = row;
    }

}

export function compareRowCol(p1, p2) {
    return ((p1.row ?? p1.line) < (p2.row ?? p2.line))
        ? -1
        : (p1.row ?? p1.line) > (p2.row ?? p2.line)
        ? 1
        : (p1.col ?? p1.c1) - (p2.col ?? p2.c1);
}

export function getPP(tok) {
    var pp = tok.context.passedParens;
    pp = pp instanceof Function ? pp() : pp;
    if (pp) pp = [...pp].sort(compareRowCol);
    return pp;
}

export function caretInside(caret, useOuter) {
    return p => (p.closed &&
                 useOuter && p.outer ? (
                     compareRowCol({ line: p.outer.l1, col: p.outer.c1 }, caret) < 0
                         && compareRowCol({ line: p.outer.l2, col: p.outer.c2 }, caret) >= 0
                 ) : (
                     compareRowCol(p, caret) < 0
                         && compareRowCol(p.closed, caret) >= 0
                 ));
}
