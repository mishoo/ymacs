//> This file is part of Ymacs, an Emacs-like editor for the Web
//> http://www.ymacs.org/
//>
//> Copyright (c) 2009-2012, Mihai Bazon, Dynarch.com.  All rights reserved.
//>
//> Redistribution and use in source and binary forms, with or without
//> modification, are permitted provided that the following conditions are
//> met:
//>
//>     * Redistributions of source code must retain the above copyright
//>       notice, this list of conditions and the following disclaimer.
//>
//>     * Redistributions in binary form must reproduce the above copyright
//>       notice, this list of conditions and the following disclaimer in
//>       the documentation and/or other materials provided with the
//>       distribution.
//>
//>     * Neither the name of Dynarch.com nor the names of its contributors
//>       may be used to endorse or promote products derived from this
//>       software without specific prior written permission.
//>
//> THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER “AS IS” AND ANY
//> EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
//> IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
//> PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER BE LIABLE
//> FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
//> CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
//> SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
//> INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
//> CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
//> ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
//> THE POSSIBILITY OF SUCH DAMAGE.

// @require ymacs-tokenizer.js

Ymacs_Tokenizer.define("markdown", function(stream, tok) {

    var PARSER = { next: next, copy: copy };

    function copy() {
        var context = restore.context = {
        };
        function restore() {
            return PARSER;
        };
        return restore;
    };

    function foundToken(c1, c2, type) {
        tok.onToken(stream.line, c1, c2, type);
    };

    function next() {
        stream.checkStop();
        var tmp;
        if (stream.col == 0 && (tmp = stream.lookingAt(/^(#+)/))) {
            foundToken(0, stream.col = stream.lineLength(), "markdown-heading" + tmp[0].length);
        }
        else if (stream.line > 0 && stream.col == 0 && (tmp = stream.lookingAt(/^[=-]+$/)) && /\S/.test(stream.lineText(stream.line - 1))) {
            tmp = tmp[0].charAt(0) == "=" ? 1 : 2;
            tmp = "markdown-heading" + tmp;
            tok.onToken(stream.line - 1, 0, stream.lineLength(stream.line - 1), tmp);
            foundToken(0, stream.col = stream.lineLength(), tmp);
        }
        else if (stream.col == 0 && (tmp = stream.lookingAt(/^[>\s]*/))) {
            tmp = tmp[0].replace(/\s+/g, "").length;
            if (tmp > 3)
                tmp = "";
            tmp = "markdown-blockquote" + tmp;
            foundToken(0, stream.col = stream.lineLength(), tmp);
        }
        else {
            foundToken(stream.col, ++stream.col, null);
        }
    };

    return PARSER;

});

Ymacs_Buffer.newMode("markdown_mode", function() {

    var tok = this.tokenizer;
    this.setTokenizer(new Ymacs_Tokenizer({ buffer: this, type: "markdown" }));
    return function() {
        this.setTokenizer(tok);
    };

});
