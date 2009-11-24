// This file is part of Ymacs, an extensible source code editor
// (c) Mihai Bazon 2009 <mihai.bazon@gmail.com>
// Distributed under a BSD-style license.
// http://www.ymacs.org/

// @require ymacs-buffer.js

DEFINE_CLASS("Ymacs_Stream", null, function(D, P){

        D.DEFAULT_ARGS = {
                buffer : [ "buffer" , null ],
                line   : [ "line"   , 0 ],
                col    : [ "col"    , 0 ]
        };

        P.nextCol = function() {
                ++this.col;
        };

        P.prevCol = function() {
                --this.col;
        };

        P.nextLine = function() {
                ++this.line;
                this.col = 0;
        };

        P.prevLine = function() {
                --this.line;
                this.col = 0;
        };

        P.peek = function(n) {
                if (n == null) n = 0;
                return this.buffer.code[this.line].charAt(this.col + n);
        };

        P.get = function() {
                var ch = this.peek();
                this.nextCol();
                return ch;
        };

        P.lineText = function(row) {
                if (row == null)
                        row = this.line;
                return this.buffer.code[row];
        };

        P.lineIndentation = function(row) {
                return /^\s*/.exec(this.lineText(row))[0].length;
        };

        P.lookingAt = function(what) {
                var line = this.buffer.code[this.line];
                if (what instanceof RegExp) {
                        return what.exec(line.substr(this.col));
                } else {
                        return line.substr(this.col, what.length) == what;
                }
        };

        P.textBefore = function(pos) {
                if (pos == null)
                        pos = this.buffer._rowColToPosition(this.line, this.col);
                return this.buffer.getCode().substr(0, pos);
        };

        P.textAfter = function(pos) {
                if (pos == null)
                        pos = this.buffer._rowColToPosition(this.line, this.col);
                return this.buffer.getCode().substr(pos);
        };

        P.substring = function(start, end) {
                return this.buffer.getCode().substring(start, end);
        };

        P.substr = function(start, end) {
                return this.buffer.getCode().substr(start, end);
        };

        P.eol = function() {
                return this.col == this.buffer.code[this.line].length;
        };

        P.eof = function() {
                var n = this.buffer.code.length, l = this.line;
                return l >= n || l == n - 1 && this.eol();
        };

        P.length = function() {
                return this.buffer.code.length;
        };

        P.lineLength = function(line) {
                if (line == null)
                        line = this.line;
                return this.buffer.code[line].length;
        };

        P.save = function() {
                return { buffer: this.buffer, line: this.line, col: this.col };
        };

        P.restore = function(state) {
                this.buffer = state.buffer;
                this.line = state.line;
                this.col = state.col;
        };

        P.checkStop = function() {
                if (this.eof()) throw this.EOF;
                if (this.eol()) throw this.EOL;
        };

        P.EOL = new (function(){});

        P.EOF = new (function(){});

});

DEFINE_CLASS("Ymacs_Tokenizer", DlEventProxy, function(D, P){

        var LANGUAGES = {};

        D.define = function(name, func) {
                LANGUAGES[name.toLowerCase()] = func;
        };

        D.DEFAULT_EVENTS = [ "onFoundToken" ];

        D.DEFAULT_ARGS = {
                buffer : [ "buffer", null ],
                type   : [ "type", null ]
        };

        D.FIXARGS = function(args) {
                if (typeof args.type == "string")
                        args.type = LANGUAGES[args.type.toLowerCase()];
        };

        D.CONSTRUCT = function() {
                var smallest = null;
                var timer = null;
                this.quickUpdate = function(offset) {
                        var row = this.buffer._positionToRowCol(offset).row;
                        this.parsers.splice(row - 1, this.parsers.length + 1);

                        if (smallest != null) {
                                smallest = Math.min(row, smallest);
                        } else {
                                smallest = row;
                        }
                        clearTimeout(timer);
                        timer = function(){
                                this._do_quickUpdate(smallest);
                                smallest = null;
                        }.delayed(1, this);
                };
                this._stopQuickUpdate = function() {
                        clearTimeout(timer);
                        clearTimeout(this.timerUpdate);
                };
                this.reset();
        };

        P.reset = function() {
                this.stream = new Ymacs_Stream({ buffer: this.buffer });
                this.theParser = this.type(this.stream, this);
                this.parsers = [];
                this.parsers[-1] = this.theParser.copy();
                this.timerUpdate = null;
                this.quickUpdate(0);
        };

        P.showProgress = function(p) {
                if (p != null) {
                        p = Math.round(p / this.stream.length() * 100) + "%";
                }
                this.buffer.whenYmacs("updateProgress", "Syntax highlighting", p);
        };

        P._do_quickUpdate = function(row) {
                this._stopQuickUpdate();
                var s = this.stream, p, a = this.parsers, n;
                s.line = row - 1;
                while (!(p = a[s.line]))
                        s.prevLine();
                s.nextLine();
                p = p();
                var iteration = 0;
                var first = true;
                var doit = function() {
                        this.buffer.preventUpdates();
                        n = first ? 2 : 20;
                        if (++iteration > 10)
                                this.showProgress(this.stream.line);
                        while (true) {
                                try {
                                        while (true) p.next();
                                }
                                catch(ex) {
                                        if (ex === s.EOL) {
                                                a[s.line] = p.copy();
                                                s.nextLine();
                                                if  (--n == 0) {
                                                        // this.buffer.whenActiveFrame("centerOnLine", s.line);
                                                        this.buffer.resumeUpdates();
                                                        this.timerUpdate = setTimeout(doit, first ? 500 : 50);
                                                        first = false;
                                                        return;
                                                }
                                        }
                                        else if (ex === s.EOF) {
                                                a[s.line] = p.copy();
                                                this.buffer.resumeUpdates();
                                                if (p.on_EOF)
                                                        p.on_EOF();
                                                break;
                                        }
                                        else throw ex;
                                }
                        }
                        this.showProgress();
                }.$(this);
                doit();
        };

        P.quickInsertLine = function(row) {
                // console.log("splicing at %d for insert line", row - 1);
                this.parsers.splice(row - 1, this.parsers.length + 1);
        };

        P.quickDeleteLine = function(row) {
                // console.log("splicing at %d for delete line", row - 1);
                this.parsers.splice(row - 1, this.parsers.length + 1);
        };

        P.onToken = function(line, c1, c2, type) {
                this.callHooks("onFoundToken", line, c1, c2, type);
        };

        P.getParserForLine = function(row) {
                this._stopQuickUpdate();
                var s = this.stream, p, a = this.parsers, n;
                var currentLine = s.line;
                s.line = row - 1;
                while (!(p = a[s.line]))
                        s.prevLine();
                s.nextLine();
                p = p();
                try {
                        this.buffer.preventUpdates();
                        while (true) {
                                if (s.line == row) {
                                        return p;
                                }
                                try {
                                        while (true) p.next();
                                } catch(ex) {
                                        if (ex === s.EOL) {
                                                a[s.line] = p.copy();
                                                s.nextLine();
                                        }
                                        else if (ex === s.EOF) {
                                                break;
                                        }
                                        else {
                                                throw ex;
                                        }
                                }
                        }
                } finally {
                        this.buffer.resumeUpdates();
                        if (currentLine < s.length()) {
                                // resume lazy tokenizer if it was interrupted
                                this.timerUpdate = this._do_quickUpdate.delayed(50, this, Math.max(row, currentLine));
                        }
                }
        };

        P.getLastParser = function() {
                return this.parsers.peek();
        };

        P.getIndentation = function(row) {
                var p = this.getParserForLine(row);
                if (p && p.indentation instanceof Function)
                        return p.indentation();
        };

});
