DEFINE_CLASS("Ymacs_Tokenizer", DlEventProxy, function(D, P){

        D.DEFAULT_EVENTS = [ "onFoundToken" ];

        D.DEFAULT_ARGS = {
                buffer: [ "buffer", null ]
        };

        D.CONSTRUCT = function() {
                this.continuations = [];
                this.line = -1;
                this.col = 0;
                this.makeContinuation(this.readToken);
                this.timerUpdate = null;
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
        };

        P.update = function(line) {
                this.stopUpdate();
                var cont;
                for (var i = line; i >= 0; --i) {
                        cont = this.continuations[line];
                        if (cont)
                                break;
                }
                var doit = function() {
                        var n = 10;
                        while (i < this.continuations.length) {
                                if (n == 0) {
                                        this.timerUpdate = setTimeout(doit, 100);
                                        break;
                                }
                                // if (this.continuations[i]()) {
                                //         this.stopUpdate();
                                //         break;
                                // }
                                this.continuations[i]();
                                i++; n--;
                        }
                }.$(this);
                // if (!cont()) {
                //         i++;
                //         this.timerUpdate = setTimeout(doit, 1000);
                // }
                cont();
                i++;
                this.timerUpdate = setTimeout(doit, 1000);
        };

        P.next = function() {
                this.col++;
        };

        P.peek = function() {
                return this.buffer.code[this.line].charAt(this.col);
        };

        P.lookingAt = function(what) {
                return this.buffer.code[this.line].substr(this.col, what.length) == what;
        };

        P.eol = function() {
                return this.col == this.buffer.code[this.line].length;
        };

        P.eof = function() {
                var n = this.buffer.code.length, l = this.line;
                return l >= n || l == n - 1 && this.eol();
        };

        P.makeContinuation = function(cont) {
                var line = ++this.line,
                    col = this.col = 0,
                    args = Array.$(arguments, 1);
                this.continuations[line] = function() {
                        this.line = line;
                        this.col = col;
                        this.buffer._textProperties[line] = null;
                        return cont.apply(this, args) && this.readToken();
                }.$(this);
                this.continuations.splice(line + 1, this.continuations.length);
        };

        P.quickUpdate = function(offset, delta) {
                if (delta < 0)
                        offset += delta;
                var line = this.buffer._positionToRowCol(offset).row;
                this.update(line);
        };

        P.highlightLine = function(row) {
                return this.buffer.code[row].htmlEscape();
        };

        P.onToken = function(c1, c2, type) {
                this.callHooks("onFoundToken", this.line, c1, c2, type);
        };

});

DEFINE_CLASS("Ymacs_Tokenizer_JS", Ymacs_Tokenizer, function(D, P){

        P.IDENTIFIER_CHARS = "$_0123456789".split("").toHash(true);
        P.IDENTIFIER_START = "$_".split("").toHash(true);

        P.readMultilineComment = function() {
                var line = this.buffer.code[this.line];
                if (line != null) {
                        var pos = line.indexOf("*/", this.col);
                        if (pos >= 0) {
                                this.onToken(this.col, pos, "mcomment");
                                this.onToken(pos, pos + 2, "mcomment-stopper");
                                this.col = pos + 2;
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

        P.readString = function(end) {
                var line = this.buffer.code[this.line], pos = this.col, esc = false, ch;
                while (line != null) {
                        ch = line.charAt(pos);
                        if (!ch) {
                                // end of line
                                this.onToken(this.col, pos, "string");
                                this.makeContinuation(this.readString, end);
                                return false;
                        }
                        if (ch === end && !esc) {
                                this.onToken(this.col, pos, "string");
                                this.onToken(pos, pos + 1, "string-stopper");
                                this.col = pos + 1;
                                return true;
                        }
                        esc = (ch === "\\" && !esc);
                        ++pos;
                };
        };

        P.readIdentifier = function() {
                var start = this.col, id = "";
                while (!this.eof() && this.isIdentifierChar()) {
                        id += this.peek();
                        this.next();
                }
                var type = id in this.KEYWORDS ? "keyword"
                        : id in this.KEYWORDS_TYPE ? "type"
                        : id in this.KEYWORDS_CONST ? "constant"
                        : id in this.BUILTIN ? "builtin"
                        : "variable";
                this.onToken(start, this.col, type);
                return true;
        };

        P.readToken = function() {
                while (!this.eof()) {
                        var ch = this.peek();
                        if (!ch) {
                                // eol
                                if (!this.eof()) {
                                        this.makeContinuation(this.readToken);
                                        return true;
                                }
                                return true;
                        }

                        if (ch == "\"" || ch == "\'") {
                                this.onToken(this.col, this.col + 1, "string-starter");
                                this.col++;
                                if (!this.readString(ch)) {
                                        return false;
                                }
                        } else if (this.lookingAt("/*")) {
                                this.onToken(this.col, this.col + 2, "mcomment-starter");
                                this.col += 2;
                                if (!this.readMultilineComment()) {
                                        return false;
                                }
                        } else if (this.lookingAt("//")) {
                                this.onToken(this.col, this.col + 2, "comment-starter");
                                this.col += 2;
                                this.readComment();
                        } else if (this.isIdentifierStart()) {
                                this.readIdentifier();
                        }

                        else {
                                this.onToken(this.col, this.col + 1, "clean");
                                this.next();
                        }
                }
                return true;
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

        // reserved names

        P.KEYWORDS = "abstract break case catch class const \
continue debugger default delete do else \
enum export extends final finally for \
function goto if implements import in \
instanceof interface native new package \
private protected public return static \
super switch synchronized throw \
throws transient try typeof var void let \
yield volatile while with".trim().split(/\s+/).toHash(true);

        P.KEYWORDS_TYPE = "boolean byte char double float int long short void".trim().split(/\s+/).toHash(true);

        P.KEYWORDS_CONST = "false null undefined Infinity NaN true arguments this".trim().split(/\s+/).toHash(true);

        P.BUILTIN = "$ Array Date Function Infinity Math NaN Number \
Object Packages RegExp String decodeURI decodeURIComponent \
encodeURI encodeURIComponent eval isFinite isNaN parseFloat \
parseInt undefined window document".trim().split(/\s+/).toHash(true);

});
