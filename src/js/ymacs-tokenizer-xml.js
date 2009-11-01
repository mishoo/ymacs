DEFINE_CLASS("Ymacs_Tokenizer_XML", Ymacs_Tokenizer, function(D, P){

        P.IDENTIFIER_START = Object.makeCopy(P.IDENTIFIER_START);
        Object.merge(P.IDENTIFIER_START, {
                ":" : true
        });

        P.IDENTIFIER_CHARS = Object.makeCopy(P.IDENTIFIER_CHARS);
        Object.merge(P.IDENTIFIER_CHARS, ":-.".split("").toHash(true));

        P.MLC_STARTER = "<!--";
        P.MLC_STOPPER = "-->";

        P.readTag = function() {
                while (!this.eof()) {
                        var ch = this.peek();
                        if (!ch) {
                                // eol
                                this.makeContinuation(this.readTag);
                                return false;
                        }
                        if (this.lookingAt(/^\x2f>/)) {
                                this.onToken(this.col, ++this.col, "xml-shorttag-slash");
                                this.onToken(this.col, ++this.col, "xml-close-bracket");
                                return true;
                        }
                        else if (ch == ">") {
                                this.onToken(this.col, ++this.col, "xml-close-bracket");
                                return true;
                        }
                        else if (this.isIdentifierStart() && this.readIdentifier("xml-attribute")) {
                        }
                        else if (ch in this.STRING_CHARS) {
                                this.onToken(this.col, this.col + ch.length, "string-starter");
                                this.col += ch.length;
                                this.nextReader = this.readTag;
                                if (!this.readString(this.STRING_CHARS[ch], "string", true)) {
                                        return false;
                                }
                                this.nextReader = this.readToken;
                        }
                        else if (ch != "!" && ch != "=" && !/\s/.test(ch)) {
                                // anything else not allowed here
                                this.onToken(this.col, ++this.col, "error");
                        }
                        else {
                                this.onToken(this.col, ++this.col, null);
                        }
                };
        };

        P.readEndTag = function() {
                var id = this.readIdentifier("xml-close-tag");
                if (this.peek() == ">") {
                        this.onToken(this.col, ++this.col, "xml-close-bracket");
                }
                return true;
        };

        P.readToken = function() {
                var m;
                while (!this.eof()) {
                        var ch = this.peek();
                        if (!ch) {
                                this.makeContinuationNofollow(this.readToken);
                                return true;
                        }
                        if (this.lookingAt(this.MLC_STARTER)) {
                                this.onToken(this.col, this.col + this.MLC_STARTER.length, "mcomment-starter");
                                this.col += this.MLC_STARTER.length;
                                if (!this.readMultilineComment()) {
                                        return false;
                                }
                        }
                        else if (this.lookingAt(/^<\x2f/)) {
                                this.onToken(this.col, ++this.col, "xml-open-bracket");
                                this.onToken(this.col, ++this.col, "xml-shorttag-slash");
                                this.readEndTag();
                        }
                        else if (ch == "<") {
                                this.onToken(this.col, ++this.col, "xml-open-bracket");
                                this.readIdentifier("xml-open-tag");
                                if (!this.readTag())
                                        return false;
                        }
                        else if ((m = this.lookingAt(/^&.*?;/))) {
                                this.onToken(this.col, ++this.col, "xml-entity-starter");
                                this.onToken(this.col, this.col += m[0].length - 2, "xml-entity");
                                this.onToken(this.col, ++this.col, "xml-entity-stopper");
                        }
                        else if (ch == "&") {
                                this.onToken(this.col, ++this.col, "error");
                        }
                        else if (!this.readMore()) {
                                this.onToken(this.col, this.col + 1, null);
                                this.nextCol();
                        }
                }
        };

});
