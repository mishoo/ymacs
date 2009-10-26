DEFINE_CLASS("Ymacs_Tokenizer", DlEventProxy, function(D, P){

        function CMP_INT(a, b) { return parseInt(a, 10) - parseInt(b, 10); };

        D.DEFAULT_ARGS = [ "onChange" ];

        D.DEFAULT_ARGS = {
                buffer : [ "buffer"  , null ],
                pos    : [ "pos", 0 ]
        };

        D.CONSTRUCT = function() {
                this.syntax = [];
                this.parens = [];
                this._do_quickUpdate = this._do_quickUpdate.clearingTimeout(50, this);
        };

        P.WHITE_SPACE = " \xa0\t\n\r".split("").toHash(true);
        P.IDENTIFIER_CHARS = "$_0123456789".split("").toHash(true);
        P.IDENTIFIER_START = "$_".split("").toHash(true);
        P.LINE_COMMENT = "//";
        P.MULTILINE_COMMENT_START = "/*";
        P.MULTILINE_COMMENT_END = "*/";
        P.STRING = "\"";
        P.STRING2 = "'";
        P.OPERATORS = "-+=/!%^&|*<>:?,".split("").toHash(true);
        P.KEYWORDS = {};
        P.KEYWORDS_TYPE = {};
        P.KEYWORDS_CONST = {};
        P.BUILTIN = {};
        P.OPEN_PAREN = "{[(".split("").toHash(true);
        P.CLOSE_PAREN = ")]}".split("").toHash(true);
        P.MATCH_PAREN = { "{" : "}", "[" : "]", "(" : ")" };

        P.code = function() {
                return this.buffer.getCode();
        };

        P.get = function() {
                return this.code().charAt(this.pos++);
        };

        P.next = function() {
                this.pos++;
        };

        P.eol = function() {
                return this.peek() == "\n";
        };

        P.eof = function() {
                return this.pos >= this.code().length;
        };

        P.peek = function() {
                return this.code().charAt(this.pos);
        };

        P.back = function() {
                this.pos--;
        };

        P.forward = function(n) {
                this.pos += n;
        };

        P.lookingAt = function(what) {
                if (what instanceof RegExp) {
                        what.lastIndex = this.pos;
                        var m = what.exec(this.code());
                        return m && m.index == this.pos && what.lastIndex;
                } else if (typeof what == "object") {
                        for (i in what)
                                if (this.lookingAt(i))
                                        return i;
                } else {
                        return this.code().substr(this.pos, what.length) == what && this.pos + what.length;
                }
        };

        P.isIdentifierChar = function(ch) {
                if (ch == null)
                        ch = this.peek();
                return ch && (ch.toUpperCase() != ch.toLowerCase() || ch in this.IDENTIFIER_CHARS);
        };

        P.isIdentifierStart = function(ch) {
                if (ch == null)
                        ch = this.peek();
                return ch && (ch.toUpperCase() != ch.toLowerCase() || ch in this.IDENTIFIER_START);
        };

        P.isOperatorChar = function() {
                return this.peek() in this.OPERATORS;
        };

        P.consumeString = function(end, esc) {
                var escape = false, ch;
                if (arguments.length == 1)
                        esc = "\\";
                while (!this.eof()) {
                        ch = this.get();
                        if (ch === end && !escape)
                                break;
                        escape = (ch === esc && !escape);
                }
                this.back();
        };

        P.consumeLineComment = function() {
                var pos = this.code().indexOf("\n", this.pos);
                this.pos = pos >= 0 ? pos : this.code().length;
        };

        P.consumeMultilineComment = function(end) {
                var pos = this.code().indexOf(end, this.pos);
                this.pos = pos >= 0 ? pos : this.code().length;
        };

        P.consumeNumber = function() {
                var hasdot = false, hase = false, hex = false, ch, code;
                while (!this.eof()) {
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
                        this.next();
                }
        };

        P.consumeOperator = function() {
                while (!this.eof()) {
                        var ch = this.peek();
                        if (ch in this.OPERATORS)
                                this.next();
                        else
                                break;
                }
        };

        P.consumeIdentifier = function() {
                var id = "";
                while (!this.eof()) {
                        if (this.isIdentifierChar()) {
                                id += this.get();
                        } else
                                break;
                }
                return id;
        };

        P.readToken = function() {
                var type, info = { begin: this.pos };
                if (this.lookingAt(this.LINE_COMMENT)) {
                        type = "line-comment";
                        this.consumeLineComment();
                        this.next(); // skip newline

                } else if (this.lookingAt(this.MULTILINE_COMMENT_START)) {
                        type = "multiline-comment";
                        this.consumeMultilineComment(this.MULTILINE_COMMENT_END);
                        this.forward(this.MULTILINE_COMMENT_END.length);

                } else if (this.lookingAt(this.STRING)) {
                        this.next(); // skip start quote
                        type = "string";
                        this.consumeString(this.STRING);
                        this.next(); // skip end quote

                } else if (this.STRING2 && this.lookingAt(this.STRING2)) {
                        this.next(); // skip start quote
                        type = "string";
                        this.consumeString(this.STRING2);
                        this.next(); // skip end quote

                } else if (this.isIdentifierStart()) {
                        var id = info.name = this.consumeIdentifier();
                        if (id in this.KEYWORDS)
                                type = "keyword";
                        else if (id in this.KEYWORDS_TYPE)
                                type = "type";
                        else if (id in this.KEYWORDS_CONST)
                                type = "constant";
                        else if (id in this.BUILTIN)
                                type = "builtin";
                        else
                                type = "identifier";

                } else if (this.isOperatorChar()) {
                        this.consumeOperator();
                        type = "operator";

                } else if (this.peek() in this.OPEN_PAREN) {
                        info.paren = this.get();
                        type = "open-paren";
                        this.parens.push(info);

                } else if (this.peek() in this.CLOSE_PAREN) {
                        var last = this.parens.pop();
                        info.paren = this.get();
                        type = "close-paren";
                        if (last) {
                                info.match = last;
                                last.match = info;
                                if (this.MATCH_PAREN[last.paren] === info.paren)
                                        info.ok = last.ok = true;
                                else {
                                        type += " fontlock-error";
                                        last.type += " fontlock-error";
                                }
                        } else {
                                type += " fontlock-error";
                        }

                } else if (this.lookingAt(/[0-9]|\.[0-9]/)) {
                        this.consumeNumber();
                        type = "number";
                }

                if (type) {
                        info.type = type;
                        info.end = this.eof() ? this.code().length : this.pos;
                        info.len = info.end - info.begin;
                        return info;
                }
        };

        P.tokenize = function(from, limit) {
                this.pos = from || 0;
                var changes = { min: this.code().length, max: 0 };
                while (!this.eof() && (!limit || this.pos <= changes.max)) {
                        while (!this.eof() && this.peek() in this.WHITE_SPACE) {
                                var old = this.syntax[this.pos];
                                if (old) {
                                        changes.min = Math.min(changes.min, old.begin);
                                        changes.max = Math.max(changes.max, old.end);
                                }
                                this.syntax[this.pos] = null;
                                this.next(); // skip characters we didn't match
                        }
                        var info = this.readToken();
                        if (info) {
                                var max = changes.max, min = changes.min, old;
                                for (var i = info.begin; i < info.end; ++i) {
                                        old = this.syntax[i];
                                        if (old) {
                                                min = Math.min(min, old.begin);
                                                max = Math.max(max, old.end);
                                        }
                                        this.syntax[i] = info;
                                }
                                changes.min = Math.min(min, info.begin);
                                changes.max = Math.max(max, info.end);
                        } else {
                                var old = this.syntax[this.pos];
                                if (old) {
                                        changes.min = Math.min(changes.min, old.begin);
                                        changes.max = Math.max(changes.max, old.end);
                                }
                                this.syntax[this.pos] = null;
                                this.next(); // skip characters we didn't match
                        }
                }
                changes.min = this.buffer._positionToRowCol(changes.min).row;
                changes.max = this.buffer._positionToRowCol(changes.max).row;
                return changes;
        };

        // P.getSyntax = function() {
        //         var a = Array.hashKeys(this.syntax);
        //         a.sort(CMP_INT);
        //         return a.map(function(key){
        //                 return this[key];
        //         }, this.syntax);
        // };

        P.highlightLine = function(line) {
                var text = this.code(),
                    lineText = this.buffer.code[line];

                var begin = this.buffer._rowColToPosition(line, 0),
                    end = begin + lineText.length,
                    buf = String.buffer(),
                    syntax = this.syntax;
                for (var i = begin; i < end;) {
                        var s = syntax[i];
                        if (s) {
                                var tmp = Math.min(i + s.len, end);
                                buf("<span class='fontlock-", s.type, "'>",
                                    text.substring(i, tmp).htmlEscape(),
                                    "</span>");
                                i = tmp;
                        } else {
                                buf(text.charAt(i).htmlEscape());
                                ++i;
                        }
                }
                return buf.get();
        };

        P.reset = function() {
                // this.syntax = [];
                this.parens = [];
                return this.tokenize();
        };

        P.quickUpdate = function(offset, delta) {
                // console.log("offset: %o, delta: %o", offset, delta);
                if (delta < 0) {
                        this.syntax.splice(offset, -delta);
                        offset += delta;
                } else if (delta > 0) {
                        var a = [ offset, 0 ];
                        var s = this.syntax[offset];
                        if (s) {
                                s.end += delta;
                                s.len += delta;
                        }
                        delta.times(function(){
                                this.push(s);
                        }, a);
                        this.syntax.splice.apply(this.syntax, a);
                }
                if (this._quickUpdateOffset == null)
                        this._quickUpdateOffset = offset;
                else
                        this._quickUpdateOffset = Math.min(offset, this._quickUpdateOffset);
                this._do_quickUpdate();
        };

        P._do_quickUpdate = function() {
                var offset = this._quickUpdateOffset;
                var syntax = this.syntax[offset];
                if (syntax)
                        offset = syntax.begin;
                else
                        offset = this.buffer._rowColToPosition(this.buffer._positionToRowCol(offset).row, 0);
                var changed = this.tokenize(offset, true), i = changed.min, n = changed.max;
                console.log(changed);
                while (i <= n)
                        this.buffer.callHooks("onLineChange", i++, true);
                this._quickUpdateOffset = null;
        };

        P.quickInsertLine = function(row) {
        };

        P.quickDeleteLine = function(row) {
        };

});

/* -----[ JavaScript tokenizer ]----- */

DEFINE_CLASS("Ymacs_Tokenizer_JS", Ymacs_Tokenizer, function(D, P){

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

        P.BUILTIN = "Array Date Function Infinity Math NaN Number \
Object Packages RegExp String decodeURI decodeURIComponent \
encodeURI encodeURIComponent eval isFinite isNaN parseFloat \
parseInt undefined window document".trim().split(/\s+/).toHash(true);

});
