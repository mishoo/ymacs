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

    P.next = function() {
        var ch = this.peek();
        this.nextCol();
        if (this.col >= this.buffer.code[this.line].length)
            this.nextLine();
        return ch;
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

    P.EOL = {};

    P.EOF = {};

    P.skip_ws = function() {
        while (Ymacs_Simple_Stream.is_whitespace(this.peek()))
            this.next();
    };

});

DEFINE_CLASS("Ymacs_Simple_Stream", null, function(D, P){
    D.DEFAULT_ARGS = {
        buffer : [ "buffer" , null ],
        line   : [ "line"   , 0 ],
        col    : [ "col"    , 0 ],
        pos    : [ "pos"    , null ]
    };
    D.CONSTRUCT = function() {
        if (this.pos == null)
            this.pos = this.buffer._rowColToPosition(this.line, this.col);
        else {
            var rc = this.buffer._positionToRowCol(this.pos);
            this.line = rc.row;
            this.col = rc.col;
        }
    };
    P.peek = function() {
        var a = this.buffer.code;
        var line = a[this.line];
        if (line == null) return null;
        if (this.col == line.length)
            return this.line == a.length - 1 ? null : "\n";
        return line.charAt(this.col);
    };
    P.next = function() {
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
    };
    P.read_while = function(pred) {
        var ret = "", ch;
        while ((ch = this.peek()) && pred(ch))
            ret += this.next();
        return ret;
    };
    P.is_whitespace = D.is_whitespace = function(ch) {
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
    };
    P.skip_ws = function() {
        return this.read_while(this.is_whitespace);
    };
    P.looking_at = function(what) {
        var line = this.buffer.code[this.line];
        if (what instanceof RegExp) {
            return what.exec(line.substr(this.col));
        } else {
            return line.substr(this.col, what.length) == what;
        }
    };
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

    P.getLanguage = function(name, options) {
        return LANGUAGES[name](this.stream, this, options);
    };

    P.showProgress = function(p) {
        if (p != null) {
            p = Math.round(p / this.stream.length() * 100) + "%";
        }
        this.buffer.updateProgress("Syntax highlighting", p);
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
            n = 100;
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
                            this.buffer.resumeUpdates();
                            this.timerUpdate = setTimeout(doit, 25);
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
        this.parsers.splice(row, this.parsers.length + 1);
    };

    P.quickDeleteLine = function(row) {
        this.parsers.splice(row, this.parsers.length + 1);
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
            // if (currentLine < s.length()) {
            //         // resume lazy tokenizer if it was interrupted
            //         this.timerUpdate = this._do_quickUpdate.delayed(50, this, Math.min(row, currentLine));
            // }
            if (s.line < s.length())
                this.timerUpdate = this._do_quickUpdate.delayed(50, this, s.line);
        }
    };

    P.reparseAll = function() {
        this.parsers.splice(0, this.parsers.length);
        return this.finishParsing();
    };

    P.finishParsing = function() {
        this.getParserForLine(this.stream.length());
        return this.getLastParser();
    };

    P.getLastParser = function() {
        return this.parsers.peek();
    };

    P.getIndentation = function(row, buffer) {
        var p = this.getParserForLine(row);
        if (p && p.indentation instanceof Function)
            return p.indentation(buffer);
    };

});
