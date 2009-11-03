// @require ymacs-tokenizer.js

(function(){

        var KEYWORDS = "abstract break case catch class const \
continue debugger default delete do else \
enum export extends final finally for \
function goto if implements import in \
instanceof interface native new package \
private protected public return static \
super switch synchronized throw \
throws transient try typeof var void let \
yield volatile while with".qw();

        var KEYWORDS_TYPE = "boolean byte char double float int long short void \
Array Date Function Math Number Object RegExp String".qw();

        var KEYWORDS_CONST = "false null undefined Infinity NaN true arguments this".qw();

        var KEYWORDS_BUILTIN = "Infinity NaN \
Packages decodeURI decodeURIComponent \
encodeURI encodeURIComponent eval isFinite isNaN parseFloat \
parseInt undefined window document alert prototype constructor".qw();

        function isLetter(ch) {
                return ch.toLowerCase() != ch.toUpperCase();
        };

        function isNameStart(ch) {
                return ch && (isLetter(ch) || /^[_$]$/.test(ch));
        };

        function isNameChar(ch) {
                return ch && (isLetter(ch) || /^[0-9_$]$/.test(ch));
        };

        function JS_PARSER(KEYWORDS, KEYWORDS_TYPE, KEYWORDS_CONST, KEYWORDS_BUILTIN, stream, tok) {

                var $cont = [],
                    PARSER = { next: next, copy: copy };

                function foundToken(c1, c2, type) {
                        tok.onToken(stream.line, c1, c2, type);
                };

                function readName() {
                        var col = stream.col, ch = stream.get(),
                            name = ch;
                        while (!stream.eol()) {
                                ch = stream.peek();
                                if (!isNameChar(ch))
                                        break;
                                name += ch;
                                stream.nextCol();
                        }
                        return ch && { line: stream.line, c1: col, c2: stream.col, id: name };
                };

                function readComment(type, end) {
                        var line = stream.lineText(), pos = line.indexOf(end, stream.col);
                        if (pos >= 0) {
                                $cont.pop();
                                foundToken(stream.col, pos, type);
                                foundToken(pos, pos += end.length, type + "-stopper");
                                stream.col = pos;
                        } else {
                                foundToken(stream.col, line.length, type);
                                stream.col = line.length;
                        }
                };

                function readString(end, type) {
                        var ch, esc = false, start = stream.col;
                        while (!stream.eol()) {
                                ch = stream.peek();
                                if (ch === end && !esc) {
                                        $cont.pop();
                                        foundToken(start, stream.col, type);
                                        foundToken(stream.col, ++stream.col, type + "-stopper");
                                        return true;
                                }
                                esc = !esc && ch === "\\";
                                stream.nextCol();
                        }
                        foundToken(start, stream.col, type);
                };

                function readLiteralRegexp() {
                        var m;
                        if (readString("/", "regexp") && (m = stream.lookingAt(/^[gmsiy]+/)))
                                foundToken(stream.col, stream.col += m[0].length, "regexp-modifier");
                };

                function next() {
                        stream.checkStop();
                        if ($cont.length > 0)
                                return $cont.peek()();
                        var ch = stream.peek(), m, name;
                        if (stream.lookingAt("/*")) {
                                foundToken(stream.col, stream.col += 2, "mcomment-starter");
                                $cont.push(readComment.$C("mcomment", "*/"));
                        }
                        else if (stream.lookingAt("//")) {
                                foundToken(stream.col, stream.col += 2, "comment-starter");
                                foundToken(stream.col, stream.col = stream.lineLength(), "comment");
                        }
                        else if (ch === '"' || ch === "'") {
                                foundToken(stream.col, ++stream.col, "string-starter");
                                $cont.push(readString.$C(ch, "string"));
                        }
                        else if ((m = stream.lookingAt(/^0x[0-9a-f]+|^[0-9]*\.?[0-9]+/))) {
                                foundToken(stream.col, stream.col += m[0].length, "number");
                        }
                        else if (isNameStart(ch) && (name = readName())) {
                                var type = name.id in KEYWORDS ? "keyword"
                                        : name.id in KEYWORDS_TYPE ? "type"
                                        : name.id in KEYWORDS_CONST ? "constant"
                                        : name.id in KEYWORDS_BUILTIN ? "builtin"
                                        : null;
                                foundToken(name.c1, name.c2, type);
                        }
                        else if (ch === "/" && /[\[({,;+\-*=?&!:][\x20\t\n\xa0]*$|return\s+$|typeof\s+$/.test(stream.textBefore())) {
                                foundToken(stream.col, ++stream.col, "regexp-starter");
                                $cont.push(readLiteralRegexp);
                        }
                        else {
                                foundToken(stream.col, ++stream.col, null);
                        }
                };

                function copy() {
                        var _cont = $cont.slice(0);
                        return function() {
                                $cont = _cont.slice(0);
                                return PARSER;
                        };
                };

                return PARSER;

        };

        Ymacs_Tokenizer.define("js", JS_PARSER.$C(
                KEYWORDS.toHash(true),
                KEYWORDS_TYPE.toHash(true),
                KEYWORDS_CONST.toHash(true),
                KEYWORDS_BUILTIN.toHash(true)
        ));

        /* -----[ DynarchLIB ]----- */

        var DL_KEYWORDS_BUILTIN = KEYWORDS_BUILTIN.concat("\
DEFINE_CLASS DEFINE_SINGLETON DEFINE_HIDDEN_CLASS \
DEFAULT_ARGS DEFAULT_EVENTS \
FIXARGS CONSTRUCT BEFORE_BASE FINISH_OBJECT_DEF \
D P $".qw());

        Ymacs_Tokenizer.define("js-dynarchlib", JS_PARSER.$C(
                KEYWORDS.toHash(true),
                KEYWORDS_TYPE.toHash(true),
                KEYWORDS_CONST.toHash(true),
                DL_KEYWORDS_BUILTIN.toHash(true)
        ));

})();
