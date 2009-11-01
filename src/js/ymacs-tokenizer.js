DEFINE_CLASS("Ymacs_String_Stream", null, function(D, P){

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

        P.lineText = function() {
                return this.buffer.code[this.line];
        };

        P.lookingAt = function(what) {
                var line = this.buffer.code[this.line];
                if (what instanceof RegExp) {
                        return what.exec(line.substr(this.col));
                } else {
                        return line.substr(this.col, what.length) == what;
                }
        };

        P.textBefore = function() {
                var pos = this.buffer._rowColToPosition(this.line, this.col);
                return this.buffer.getCode().substr(0, pos);
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

        P.lineLength = function() {
                return this.buffer.code[this.line].length;
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

        P.reset = function() {
                this.stream = new Ymacs_String_Stream({ buffer: this.buffer });
                this.theParser = new this.type(this.stream, this);
                this.parsers = [];
                this.parsers[-1] = this.theParser.copy();
                this.timerUpdate = null;
                this.quickUpdate(0);
        };

        P.quickUpdate = function(row) {
                var s = this.stream, p, a = this.parsers, n;
                s.line = row - 1;
                while (!(p = a[s.line]))
                        s.prevLine();
                s.nextLine();
                p = p();
                clearTimeout(this.timerUpdate);
                var first = true;
                var doit = function() {
                        n = first ? 2 : 10;
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
                                                        this.timerUpdate = setTimeout(doit, first ? 500 : 50);
                                                        first = false;
                                                        return;
                                                }
                                        }
                                        else if (ex === s.EOF) {
                                                if (p.on_EOF)
                                                        p.on_EOF();
                                                return;
                                        }
                                        else throw ex;
                                }
                        }
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

});
