DEFINE_CLASS("Ymacs_Tokenizer", DlEventProxy, function(D, P){

        D.DEFAULT_ARGS = [ "onChange" ];

        D.DEFAULT_ARGS = {
                buffer : [ "buffer"  , null ],
                line   : [ "line"    , 0 ],
                col    : [ "col"     , 0 ]
        };

        D.CONSTRUCT = function() {
                this.syntax = [];
                this.parens = [];
                this.code = this.buffer.code;
                this.quickUpdate = this.quickUpdate.clearingTimeout(50, this);
        };

        P.WHITE_SPACE = " \xa0\t\n\r".split("").toHash(true);
        P.IDENTIFIER_CHARS = "$_0123456789".split("").toHash(true);
        P.IDENTIFIER_START = "$_".split("").toHash(true);
        P.LINE_COMMENT = "//";
        P.MULTILINE_COMMENT_START = "/*";
        P.MULTILINE_COMMENT_END = "*/";
        P.STRING = "\"";
        P.STRING2 = "'";
        P.OPERATORS = "-+=/!%^&|*<>".split("").toHash(true);
        P.KEYWORDS = {};
        P.KEYWORDS_TYPE = {};
        P.KEYWORDS_CONST = {};
        P.BUILTIN = {};
        P.OPEN_PAREN = "{[(".split("").toHash(true);
        P.CLOSE_PAREN = ")]}".split("").toHash(true);
        P.MATCH_PAREN = { "{" : "}", "[" : "]", "(" : ")" };

        P.skipWhitespace = function() {
                while (!this.eof() && this.peek() in this.WHITE_SPACE)
                        this.next();
        };

        P.position = function() {
                return this.buffer._rowColToPosition(this.line, this.col);
        };

        P.get = function() {
                var line = this.code[this.line], ch = line && (this.col == line.length ? "\n" : line.charAt(this.col));
                this.next();
                return ch;
        };

        P.next = function() {
                if (this.col < this.code[this.line].length) {
                        ++this.col;
                } else {
                        ++this.line;
                        this.col = 0;
                }
        };

        P.eol = function() {
                return this.col == this.code[this.line].length;
        };

        P.eof = function() {
                return this.line >= this.code.length || (
                        this.line == this.code.length - 1 && this.col == this.code[this.line].length
                );
        };

        P.peek = function() {
                var line = this.code[this.line];
                return line && (this.col == line.length ? "\n" : line.charAt(this.col));
        };

        P.back = function() {
                if (this.col > 0) {
                        --this.col;
                } else {
                        --this.line;
                        this.col = 0;
                }
        };

        P.forward = function(n) {
                if (n == null) n = 1;
                while (!this.eof() && n-- > 0)
                        this.next();
        };

        P.lookingAt = function(what) {
                if (what instanceof RegExp) {
                        var code = this.buffer.getCode();
                        var pos = this.position();
                        what.lastIndex = pos;
                        var m = what.exec(code);
                        // console.log("looking for %o in %o from %o", what, code, pos);
                        return m && m.index == pos && what.lastIndex;
                } else if (typeof what == "object") {
                        for (i in what)
                                if (this.lookingAt(i))
                                        return i;
                } else {
                        return this.code[this.line].substr(this.col, what.length) == what && this.col + what.length;
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
                        if (ch === end && !escape) {
                                this.back();
                                break;
                        }
                        escape = (ch === esc && !escape);
                }
        };

        P.consumeLineComment = function() {
                while (!this.eof() && !this.eol())
                        this.next();
        };

        P.consumeMultilineComment = function(end) {
                while (!(this.eof() || this.lookingAt(end)))
                        this.next();
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
                this.skipWhitespace();
                var type, info = { begin: [ this.line, this.col ] };
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

                } else if (this.lookingAt(this.STRING2)) {
                        this.next(); // skip start quote
                        type = "string";
                        this.consumeString(this.STRING2);
                        this.next(); // skip end quote

                } else if (this.isIdentifierStart()) {
                        var id = this.consumeIdentifier();
                        if (id in this.KEYWORDS)
                                type = "keyword";
                        else if (id in this.KEYWORDS_TYPE)
                                type = "type";
                        else if (id in this.KEYWORDS_CONST)
                                type = "constant";
                        else if (id in this.BUILTIN)
                                type = "builtin";

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
                        if (last) {
                                info.match = last;
                                last.match = info;
                                if (this.MATCH_PAREN[last.paren] === info.paren)
                                        info.ok = last.ok = true;
                        }
                        type = "close-paren";

                } else if (this.lookingAt(/[0-9]|\.[0-9]/g)) {
                        this.consumeNumber();
                        type = "number";
                }

                if (type) {
                        info.type = type;
                        info.end = [ this.line, this.col ];
                        return info;
                }
        };

        P.add_syntax = function(info) {
                var line = info.begin[0], col = info.begin[1],
                    s = this.syntax[line] || (this.syntax[line] = {}),
                    old = s[col], removed = false;
                s[col] = info;
                for (var i = col + 1; i < info.end[1]; ++i) {
                        if (s[i]) {
                                removed = true;
                                delete s[i];
                        }
                }
                return !old || old.type != info.type || old.end[1] != info.end[1] || old.end[0] != info.end[0];
        };

        P.del_syntax = function(line, col) {
                var s = this.syntax[line];
                if (s && s[col]) {
                        delete s[col];
                        return true;
                }
        };

        P.tokenize = function() {
                this.line = 0;
                this.col = 0;
                var changes = {};
                while (!this.eof()) {
                        var info = this.readToken();
                        if (info) {
                                var i = info.begin[0], last = info.end[0];
                                if (i == last && this.add_syntax(info)) {
                                        changes[i] = true;
                                } else {
                                        var tmp = Object.makeCopy(info);
                                        while (i < last) {
                                                tmp.end = [ i, this.code[i].length ];
                                                if (this.add_syntax(tmp)) {
                                                        changes[i] = true;
                                                        this.syntax[i] = {};
                                                        this.syntax[i][tmp.begin[1]] = tmp;
                                                }
                                                tmp = Object.makeCopy(tmp);
                                                i++;
                                                tmp.begin = [ i, 0 ];
                                        }
                                        tmp.end = info.end;
                                        if (this.add_syntax(tmp))
                                                changes[i] = true;
                                }
                        } else {
                                if (this.del_syntax(this.line, this.col)) {
                                        changes[this.line] = true;
                                }
                                this.next(); // skip characters we didn't match
                        }
                }
                return changes;
        };

        P.highlightLine = function(line) {
                var syntax = this.syntax[line], text = this.code[line];
                if (!syntax)
                        return text.htmlEscape();
                var hl = "", last = 0;
                Array.hashKeys(syntax).mergeSort(function(a, b){
                        return parseInt(a) - parseInt(b);
                }).foreach(function(i){
                        var s = syntax[i];
                        if (i > last)
                                hl += text.substring(last, i);
                        hl += "<span class='fontlock-" + s.type + "'>" + text.substring(i, s.end[1]).htmlEscape() + "</span>";
                        last = s.end[1];
                });
                if (last < text.length)
                        hl += text.substr(last);
                return hl;
        };

        P.reset = function() {
                this.syntax = [];
                this.parens = [];
                this.code = this.buffer.code;
                return this.tokenize();
        };

        P.quickUpdate = function(row) {
                var changed = Array.hashKeys(this.reset());
                changed.foreach(function(row){
                        this.buffer.callHooks("onLineChange", row);
                }, this);
        };

        P.quickInsertLine = function(row) {
                this.syntax.splice(row, 0, null);
        };

        P.quickDeleteLine = function(row) {
                this.syntax.splice(row, 1);
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
