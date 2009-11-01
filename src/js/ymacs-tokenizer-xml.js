// @require ymacs-tokenizer.js

Ymacs_Tokenizer.define("xml", function(stream, tok) {

        var $tags = [],
            $cont = [],
            PARSER = { next: next, copy: copy, on_EOF: on_EOF };

        function foundToken(c1, c2, type) {
                tok.onToken(stream.line, c1, c2, type);
        };

        function isLetter(ch) {
                return ch.toLowerCase() != ch.toUpperCase();
        };

        function isNameStart(ch) {
                return ch && (isLetter(ch) || /^[:_-]$/.test(ch));
        };

        function isNameChar(ch) {
                return ch && (isLetter(ch) || /^[0-9_-]$/.test(ch));
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

        function readString(end) {
                var ch, esc = false, start = stream.col;
                while (!stream.eol()) {
                        ch = stream.peek();
                        if (ch === end && !esc) {
                                $cont.pop();
                                foundToken(start, stream.col, "string");
                                foundToken(stream.col, ++stream.col, "string-stopper");
                                return;
                        }
                        esc = !esc && ch === "\\";
                        stream.nextCol();
                }
                foundToken(start, stream.col, "string");
        };

        function readTag(tag) {
                var ch = stream.peek(), name;
                if (stream.lookingAt(/^\x2f>/)) {
                        $cont.pop();
                        foundToken(stream.col, ++stream.col, "xml-closetag-slash");
                        foundToken(stream.col, ++stream.col, "xml-close-bracket");
                }
                else if (ch === ">") {
                        $cont.pop();
                        $tags.push(tag);
                        foundToken(stream.col, ++stream.col, "xml-close-bracket");
                }
                else if (isNameStart(ch) && (name = readName())) {
                        foundToken(name.c1, name.c2, "xml-attribute");
                }
                else if (ch === '"' || ch === "'") {
                        foundToken(stream.col, ++stream.col, "string-starter");
                        $cont.push(readString.$C(ch));
                }
                else foundToken(stream.col, ++stream.col, null);
        };

        function readComment(type, end) {
                var line = stream.lineText(), pos = line.indexOf(end);
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

        function readCloseBracket() {
                var m = stream.lookingAt(/^([\s\xA0]*)(>?)/);
                if (m && m[0]) {
                        if (m[1])
                                foundToken(stream.col, stream.col += m[1].length, null);
                        if (m[2]) {
                                foundToken(stream.col, stream.col += m[2].length, "xml-close-bracket");
                                $cont.pop();
                        }
                } else {
                        foundToken(stream.col, ++stream.col, "error");
                }
        };

        function next() {
                stream.checkStop();
                if ($cont.length > 0)
                        return $cont.peek()();
                var ch = stream.peek(), m;
                if (stream.lookingAt("<![CDATA[")) {
                        foundToken(stream.col, stream.col += 9, "xml-cdata-starter");
                        $cont.push(readComment.$C("xml-cdata", "]]>"));
                }
                else if (stream.lookingAt("<!--")) {
                        foundToken(stream.col, stream.col += 4, "mcomment-starter");
                        $cont.push(readComment.$C("mcomment", "-->"));
                }
                else if (stream.lookingAt(/^<\x2f/) && isNameStart(stream.peek(+2))) {
                        foundToken(stream.col, ++stream.col, "xml-open-bracket");
                        foundToken(stream.col, ++stream.col, "xml-closetag-slash");
                        var tag = readName(), prev = $tags.pop();
                        foundToken(tag.c1, tag.c2, ( prev && prev.id == tag.id
                                                     ? "xml-close-tag"
                                                     : "error" ));
                        $cont.push(readCloseBracket);
                }
                else if (ch === "<" && isNameStart(stream.peek(+1))) {
                        foundToken(stream.col, ++stream.col, "xml-open-bracket");
                        var tag = readName();
                        foundToken(tag.c1, tag.c2, "xml-open-tag");
                        $cont.push(readTag.$C(tag));
                }
                else if ((m = stream.lookingAt(/^&.*?;/))) {
                        foundToken(stream.col, ++stream.col, "xml-entity-starter");
                        foundToken(stream.col, stream.col += m[0].length - 2, "xml-entity");
                        foundToken(stream.col, ++stream.col, "xml-entity-stopper");
                }
                else if (ch === "&") {
                        foundToken(stream.col, ++stream.col, "error");
                }
                else {
                        foundToken(stream.col, ++stream.col, null);
                }
        };

        function copy() {
                var _tags = $tags.slice(0),
                    _cont = $cont.slice(0);
                return function() {
                        $cont = _cont.slice(0);
                        $tags = _tags.slice(0);
                        return PARSER;
                };
        };

        // this isn't working too well...  It does highlight the unclosed tags, but when we fix them, it doesn't re-color.
        function on_EOF() {
                // // unclosed tags?
                // $tags.foreach(function(tag){
                //         tok.onToken(tag.line, tag.c1, tag.c2, "error");
                // });
        };

        return PARSER;

});
