DEFINE_CLASS("Ymacs_Tokenizer_JS", Ymacs_Tokenizer, function(D, P){

        P.IDENTIFIER_CHARS = "$_0123456789".split("").toHash(true);
        P.IDENTIFIER_START = "$_".split("").toHash(true);
        P.NUMBER_START = /^[0-9]|^\.[0-9]/;
        P.STRING_CHARS = { '"' : '"', "'" : "'" };
        P.ESCAPE_CHAR = "\\";
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
parseInt undefined window document alert prototype constructor".trim().split(/\s+/).toHash(true);

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

// the JS_DynarchLIB tokenizer recognizes and highlights some
// constructs that are widely used in DL.  DynarchLIB class names are
// also recognized, so long as they are actually loaded (i.e. DlWidget
// is highlighted as type only if window.DlWidget exists).  Not sure
// this was a good idea.
DEFINE_CLASS("Ymacs_Tokenizer_JS_DynarchLIB", Ymacs_Tokenizer_JS, function(D, P){

        P.BUILTIN = Object.makeCopy(P.BUILTIN);
        Object.merge(P.BUILTIN, "DEFINE_CLASS DEFINE_SINGLETON DEFINE_HIDDEN_CLASS \
DEFAULT_ARGS DEFAULT_EVENTS \
FIXARGS CONSTRUCT BEFORE_BASE FINISH_OBJECT_DEF \
D P $".split(/\s+/).toHash(true));

        P.readIdentifier = function() {
                var m;
                // DynarchLIB class name
                if ((m = this.lookingAt(/^Dl[a-zA-Z0-9$_]+/))) {
                        var id = m[0];
                        if (window[id]) {
                                this.onToken(this.col, this.col += id.length, "type");
                                return true;
                        }
                }
                return D.BASE.readIdentifier.apply(this, arguments);
        };

});
