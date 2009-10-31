// @require ymacs-tokenizer-javascript.js

DEFINE_CLASS("Ymacs_Tokenizer_XML", Ymacs_Tokenizer_JS, function(D, P){

        // XXX: we should more closely resemble this:
        // http://www.w3.org/TR/REC-xml/#NT-NameStartChar

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
                                if (!this.eof()) {
                                        // eol
                                        this.makeContinuation(this.readTag);
                                        return false;
                                }
                                return true;
                        }
                        if (ch == ">") {
                                this.onToken(this.col, ++this.col, "xml-close-bracket");
                                return true;

                        } else if (this.isIdentifierStart() &&
                                   this.lookingAt(/^[^\s]+\s*=/) &&
                                   this.readIdentifier("xml-attribute")) {
                                // nothing here, readIdentifier does the job

                        }
                };
        };

        P.readEndTag = function() {

        };

});
