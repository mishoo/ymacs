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

DEFINE_CLASS("Ymacs_Text_Properties", DlEventProxy, function(D, P){

    D.DEFAULT_EVENTS = [ "onChange" ];

    D.DEFAULT_ARGS = {
        buffer: [ "buffer", null ]
    };

    D.CONSTRUCT = P.reset = function() {
        this.props = [];
    };

    P.insertLine = function(row) {
        if (this.props.length < row)
            this.props[row] = null;
        else {
            this.props.splice(row, 0, null);
        }
    };

    P.deleteLine = function(row) {
        this.props.splice(row, 1);
    };

    P.replaceLine = function(row, text) {
        var p = this.props[row];
        if (p && p.length > text.length) {
            // remove extra-properties
            p.splice(text.length, p.length);
        }
    };

    P.addLineProps = function(row, i, j, prop, val) {
        var p = this.props, o, changed = false;
        if (i < j) {
            p = p[row] || (p[row] = []);
            while (i < j) {
                o = p[i] || (p[i] = {});
                if (o[prop] != val)
                    changed = true;
                o[prop] = val;
                ++i;
            }
            if (changed)
                this.callHooks("onChange", row);
        }
        return changed;
    };

    P.removeLineProps = function(row, i, j, prop) {
        var p = this.props[row], o, changed = false;
        if (p && i < j) {
            while (i < j) {
                o = p[i];
                if (o && prop in o) {
                    changed = true;
                    delete o[prop];
                }
                ++i;
            }
            if (changed)
                this.callHooks("onChange", row);
        }
        return changed;
    };

    // this uses the "css" text property to intercalate <span class="$css"> ... </span> tags in the given text.
    // "css" properties are added as the tokenizer parses the code and sends onFoundToken events.
    //
    // XXX: this function will be called a lot of times; seems complicated for what it does. Figure out if it can be
    // optimized.
    //
    // Update: the mess got bigger once I decided to embed the caret in the text, rather than have it absolutely
    // positioned (which seems to be the only practical way to position the cursor at the correct location).  It is
    // ESSENTIAL that the start tag of the element that defines the caret ends with "Ymacs-caret'>", so that the
    // frame widget can find it.
    P.getLineHTML = function(row, text, caret) {
        var p = this.props[row];
        if (caret === null) {
            if (text == "")
                return "<br/>";
            if (!p || p.length == 0) {
                return text.htmlEscape();
            }
        } else {
            if (text == "")
                return "<span class='Ymacs-caret'>&nbsp;</span>";
            if (!p || p.length == 0) {
                if (caret === text.length)
                    return text.htmlEscape() + "<span class='Ymacs-caret'>&nbsp;</span>";
                return text.substr(0, caret).htmlEscape() +
                    "<span class='Ymacs-caret'>" +
                    text.charAt(caret).htmlEscape() +
                    "</span>" +
                    text.substr(caret + 1).htmlEscape();
            }
        }
        var i = 0, n = text.length, last = null, o, ret = "", ch;
        while (i < n) {
            o = p[i];
            o = o && o.css;
            if (i === caret) {
                o = o ? o + " Ymacs-caret" : "Ymacs-caret";
            }
            if (o && o != last) {
                if (last)
                    ret += "</span>";
                ret += "<span class='" + o + "'>";
            }
            else if (!o && last) {
                ret += "</span>";
            }
            last = o;
            // XXX: Should have used a hash rather than a
            // switch statement?  I'm not sure but I have
            // a feeling that switch is faster.
            ch = text.charAt(i);
            switch (ch) {
              case "<" : ret += "&lt;"; break;
              case ">" : ret += "&gt;"; break;
              case "&" : ret += "&amp;"; break;
              default  : ret += ch; break;
            }
            ++i;
        }
        if (last)
            ret += "</span>";
        if (i === caret) {
            // caret is at EOL
            ret += "<span class='Ymacs-caret'>&nbsp;</span>";
        }
        return ret;
    };

});
