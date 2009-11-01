DEFINE_CLASS("Ymacs_String_Stream", DlEventProxy, function(D, P){

        D.DEFAULT_ARGS = {
                buffer: [ "buffer", null ]
        };

        D.CONSTRUCT = function() {
                this.line = 0;
                this.col = 0;
        };

        P.nextCol = function() {
                ++this.col;
        };

        P.prevCol = function() {
                --this.col;
        };

        P.peek = function() {
                return this.buffer.code[this.line].charAt(this.col);
        };

        P.lookingAt = function(what) {
                var line = this.buffer.code[this.line];
                if (what instanceof RegExp) {
                        return what.exec(line.substr(this.col));
                } else {
                        return line.substr(this.col, what.length) == what;
                }
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

});

DEFINE_CLASS("Ymacs_Tokenizer", Ymacs_String_Stream, function(D, P){

        D.DEFAULT_EVENTS = [ "onFoundToken" ];

        P.KEYWORDS = {};
        P.KEYWORDS_TYPE = {};
        P.KEYWORDS_CONST = {};
        P.KEYWORDS_BUILTIN = {};

        P.IDENTIFIER_CHARS = "$_0123456789".split("").toHash(true);
        P.IDENTIFIER_START = "$_".split("").toHash(true);
        P.NUMBER_START = /^[0-9]|^\.[0-9]/;
        P.STRING_CHARS = { '"' : '"', "'" : "'" };
        P.ESCAPE_CHAR = "\\";
        P.MLC_STARTER = null;
        P.MLC_STOPPER = null;
        P.COMMENT = null;

        D.CONSTRUCT = function() {
                this.continuations = [];
                this.line = -1;
                this.col = 0;
                this.nextReader = this.readToken;
                this.v = {};
                this.timerUpdate = null;
                this.makeContinuationNofollow(this.nextReader);
        };

        P.reset = function() {
                var a = this.continuations;
                for (var i = 0; i < a.length; ++i) {
                        a[i]();
                }
        };

        P.stopUpdate = function() {
                clearTimeout(this.timerUpdate);
                this.timerUpdate = null;
                // this.showProgress(null);
        };

        P.showProgress = function(line) {
                this.buffer.whenYmacs(function(ymacs){
                        var progress, len;
                        if (line != null) {
                                len = this.length();
                                progress = Math.round(100 * line / len) + "% [" + line + " of " + len + "]";
                        }
                        ymacs.updateProgress("Coloring", progress);
                }.$(this));
        };

        P.update = function(line) {
                this.stopUpdate();
                var cont = this.continuations, n;
                var doit = function(timeout, howMany) {
                        var func;
                        n = howMany || 10;
                        while (line < cont.length || cont.length < this.length()) {
                                while (line >= 0 && !(func = cont[line]))
                                        --line;
                                if (line < 0)
                                        break;
                                func();
                                ++line;
                                --n;
                                if (n == 0) {
                                        // DEBUG
                                        // this.buffer.whenActiveFrame("centerOnLine", line);
                                        this.timerUpdate = setTimeout(doit, timeout || 50);
                                        // this.showProgress(line);
                                        return;
                                }
                        }
                        this.stopUpdate();
                }.$(this);
                doit(500, 2);
        };

        P.makeContinuation = function(cont) {
                var line = ++this.line,
                    col  = this.col = 0,
                    vars = Object.makeCopy(this.v),
                    reader = this.nextReader,
                    args = Array.$(arguments, 1);
                return this.continuations[line] = function() {
                        this.line = line;
                        this.col = col;
                        this.v = vars;
                        this.nextReader = reader;
                        return cont.apply(this, args) && this.line == line && this.nextReader();
                }.$(this);
        };

        P.makeContinuationNofollow = function(cont) {
                var line = ++this.line,
                    col = this.col = 0,
                    vars = Object.makeCopy(this.v),
                    args = Array.$(arguments, 1);
                return this.continuations[line] = function() {
                        this.line = line;
                        this.col = col;
                        this.v = vars;
                        return cont.apply(this, args);
                }.$(this);
        };

        P.quickInsertLine = function(row) {
                // invalidate next continuations
                this.continuations.splice(row, this.continuations.length + 1);
        };

        P.quickDeleteLine = function(row) {
                // invalidate next continuations
                this.continuations.splice(row, this.continuations.length + 1);
        };

        P.quickUpdate = function(offset, delta) {
                if (delta < 0)
                        offset += delta;
                var line = this.buffer._positionToRowCol(offset).row;
                if (line > 0)
                        --line;
                this.update(line);
        };

        P.onToken = function(c1, c2, type) {
                this.callHooks("onFoundToken", this.line, c1, c2, type);
        };

        P.readMultilineComment = function() {
                var line = this.buffer.code[this.line];
                if (line != null) {
                        var pos = line.indexOf(this.MLC_STOPPER, this.col);
                        if (pos >= 0) {
                                this.onToken(this.col, pos, "mcomment");
                                this.onToken(pos, pos + this.MLC_STOPPER.length, "mcomment-stopper");
                                this.col = pos + this.MLC_STOPPER.length;
                                return true;
                        } else {
                                this.onToken(this.col, line.length, "mcomment");
                                this.makeContinuation(this.readMultilineComment);
                                return false;
                        }
                }
        };

        P.readComment = function() {
                var line = this.buffer.code[this.line];
                if (line != null) {
                        this.onToken(this.col, line.length, "comment");
                        this.col = line.length;
                        return true;
                }
        };

        P.readString = function(end, className, allowNewline) {
                var line = this.buffer.code[this.line], pos = this.col, esc = false, ch;
                while (line != null) {
                        ch = line.charAt(pos);
                        if (!ch) {
                                // end of line
                                this.onToken(this.col, pos, className);
                                if (esc || allowNewline) {
                                        this.makeContinuation(this.readString, end, className, allowNewline);
                                        return false;
                                }
                                this.col = pos;
                                return true;
                        }
                        if (ch === end && !esc) {
                                this.onToken(this.col, pos, className);
                                this.onToken(pos, pos + 1, className + "-stopper");
                                this.col = pos + 1;
                                return true;
                        }
                        esc = (ch === this.ESCAPE_CHAR && !esc);
                        ++pos;
                };
        };

        P.readIdentifier = function(defaultType) {
                var start = this.col, id = "";
                while (!this.eof() && this.isIdentifierChar()) {
                        id += this.peek();
                        this.nextCol();
                }
                var type = id in this.KEYWORDS ? "keyword"
                        : id in this.KEYWORDS_TYPE ? "type"
                        : id in this.KEYWORDS_CONST ? "constant"
                        : id in this.KEYWORDS_BUILTIN ? "builtin"
                        : defaultType;
                this.onToken(start, this.col, type);
                return id;
        };

        P.isIdentifierStart = function(ch) {
                if (ch == null)
                        ch = this.peek();
                return ch && (ch.toUpperCase() != ch.toLowerCase() || ch in this.IDENTIFIER_START);
        };

        P.isIdentifierChar = function(ch) {
                if (ch == null)
                        ch = this.peek();
                return ch && (ch.toUpperCase() != ch.toLowerCase() || ch in this.IDENTIFIER_CHARS);
        };

        P.readNumber = function() {
                var start = this.col, hasdot = false, hase = false, hex = false, ch, code;
                while (!this.eof() && !this.eol()) {
                        ch = this.peek();
                        code = ch.charCodeAt(0);
                        if ((code < 48 || code > 57) && !(hex && ((code >= 65 && code <= 70) ||
                                                                  (code >= 97 && code <= 102)))) {
                                if (ch == ".") {
                                        if (hasdot) break;
                                        hasdot = true;
                                } else if (ch == "x") {
                                        if (hex) break;
                                        hex = true;
                                } else if (ch == "e") {
                                        if (hase) break;
                                        hase = true;
                                } else break;
                        }
                        this.nextCol();
                }
                this.onToken(start, this.col, "number");
                return true;
        };

        P.readToken = function() {
                while (!this.eof()) {
                        var ch = this.peek();
                        if (!ch) {
                                // eol
                                if (!this.eof()) {
                                        // standard continuation on the next line
                                        this.makeContinuationNofollow(this.readToken);
                                        return true;
                                }
                                return true;
                        }

                        if (ch in this.STRING_CHARS) {
                                this.onToken(this.col, this.col + ch.length, "string-starter");
                                this.col += ch.length;
                                if (!this.readString(this.STRING_CHARS[ch], "string")) {
                                        return false;
                                }

                        } else if (this.MLC_STARTER && this.lookingAt(this.MLC_STARTER)) {
                                this.onToken(this.col, this.col + this.MLC_STARTER.length, "mcomment-starter");
                                this.col += this.MLC_STARTER.length;
                                if (!this.readMultilineComment()) {
                                        return false;
                                }

                        } else if (this.COMMENT && this.lookingAt(this.COMMENT)) {
                                this.onToken(this.col, this.col + this.COMMENT.length, "comment-starter");
                                this.col += this.COMMENT.length;
                                this.readComment();

                        } else if (this.lookingAt(this.NUMBER_START)) {
                                this.readNumber();

                        } else if (this.isIdentifierStart() && this.readIdentifier()) {
                                // nothing here, readIdentifier does the job

                        } else if (!this.readMore()) {
                                this.onToken(this.col, this.col + 1, null);
                                this.nextCol();
                        }
                }
                return true;
        };

        P.readMore = Function.noop;

});
