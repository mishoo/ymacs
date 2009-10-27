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
                if (!cont) {
                        this.update(0);
                        return;
                }
                var doit = function() {
                        var n = 10;
                        if (this.length())
                                this.buffer.ymacs.updateProgress("Coloring", i / this.length());
                        while (i < this.continuations.length) {
                                if (n == 0) {
                                        this.timerUpdate = setTimeout(doit, 50);
                                        return;
                                }
                                this.continuations[i]();
                                i++; n--;
                        }
                        this.buffer.ymacs.updateProgress("Coloring", null);
                }.$(this);
                cont();
                i++;
                this.timerUpdate = setTimeout(doit, 660);
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
                        esc = (ch === "\\" && !esc);
                        ++pos;
                };
        };

        P.readIdentifier = function() {
                var start = this.col, id = "";
                while (!this.eof() && this.isIdentifierChar()) {
                        id += this.peek();
                        this.nextCol();
                }
                var type = id in this.KEYWORDS ? "keyword"
                        : id in this.KEYWORDS_TYPE ? "type"
                        : id in this.KEYWORDS_CONST ? "constant"
                        : id in this.BUILTIN ? "builtin"
                        : null;
                if (type) {
                        this.onToken(start, this.col, type);
                        return true;
                }
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
                                        this.makeContinuation(this.readToken);
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

                        } else if (this.lookingAt(this.MLC_STARTER)) {
                                this.onToken(this.col, this.col + this.MLC_STARTER.length, "mcomment-starter");
                                this.col += this.MLC_STARTER.length;
                                if (!this.readMultilineComment()) {
                                        return false;
                                }

                        } else if (this.lookingAt(this.COMMENT)) {
                                this.onToken(this.col, this.col + this.COMMENT.length, "comment-starter");
                                this.col += this.COMMENT.length;
                                this.readComment();

                        } else if (this.lookingAt(this.NUMBER_START)) {
                                this.readNumber();

                        } else if (this.isIdentifierStart() && this.readIdentifier()) {

                        } else if (!this.readMore()) {
                                // this.onToken(this.col, this.col + 1, "clean");
                                this.buffer.callHooks("onLineChange", this.line);
                                this.nextCol();
                        }
                }
                return true;
        };

        P.readMore = Function.noop;

});

DEFINE_CLASS("Ymacs_Tokenizer_JS", Ymacs_Tokenizer, function(D, P){

        P.IDENTIFIER_CHARS = "$_0123456789".split("").toHash(true);
        P.IDENTIFIER_START = "$_".split("").toHash(true);
        P.NUMBER_START = /^[0-9]|^\.[0-9]/;
        P.STRING_CHARS = { '"' : '"', "'" : "'" };
        P.MLC_STARTER = "/*";
        P.MLC_STOPPER = "*/";
        P.COMMENT = "//";

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

        P.KEYWORDS_TYPE = "boolean byte char double float int long short void \
Array Date Function Math Number Object RegExp String".trim().split(/\s+/).toHash(true);

        P.KEYWORDS_CONST = "false null undefined Infinity NaN true arguments this".trim().split(/\s+/).toHash(true);

        P.BUILTIN = "Infinity NaN \
Packages decodeURI decodeURIComponent \
encodeURI encodeURIComponent eval isFinite isNaN parseFloat \
parseInt undefined window document alert".trim().split(/\s+/).toHash(true);

        P.readLiteralRegexp = function() {
                var ret = this.readString("/", "regexp"),
                    m = ret && this.lookingAt(/^[gmsiy]+/);
                if (m)
                        this.onToken(this.col, this.col += m[0].length, "regexp-modifier");
                return ret;
        };

        P.readMore = function() {
                // literal regexp
                if (this.peek() == "/") {
                        var pos = this.buffer._rowColToPosition(this.line, this.col), str = this.buffer._bufferSubstring(0, pos);
                        if (/[\[({,;+\-*=?&:][\x20\t\n\xa0]*$/.test(str)) {
                                this.onToken(this.col, ++this.col, "regexp-starter");
                                return this.readLiteralRegexp();
                        }
                }
                return D.BASE.readMore.apply(this, arguments);
        };

});

DEFINE_CLASS("Ymacs_Tokenizer_JS_DynarchLIB", Ymacs_Tokenizer_JS, function(D, P){

        P.BUILTIN = Object.makeCopy(P.BUILTIN);
        Object.merge(P.BUILTIN, "DEFINE_CLASS DEFINE_SINGLETON DEFINE_HIDDEN_CLASS \
DEFAULT_ARGS DEFAULT_EVENTS \
FIXARGS CONSTRUCT BEFORE_BASE FINISH_OBJECT_DEF \
D P $".split(/\s+/).toHash(true));

        P.readIdentifier = function() {
                var m;
                // DynarchLIB class name
                if ((m = this.lookingAt(/^Dl[a-zA-Z0-9$_]+/i))) {
                        var id = m[0];
                        if (window[id]) {
                                this.onToken(this.col, this.col += id.length, "type");
                                return true;
                        }
                }
                return D.BASE.readIdentifier.apply(this, arguments);
        };

});
