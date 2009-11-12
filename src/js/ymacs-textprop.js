// This file is part of Ymacs, an extensible source code editor
// (c) Mihai Bazon 2009 <mihai.bazon@gmail.com>
// Distributed under a BSD-style license.
// http://www.ymacs.org/

DEFINE_CLASS("Ymacs_Text_Properties", DlEventProxy, function(D, P){

        D.DEFAULT_EVENTS = [ "onChange" ];

        D.DEFAULT_ARGS = {
                buffer: [ "buffer", null ]
        };

        D.CONSTRUCT = P.reset = function() {
                this.props = [];
        };

        // inserts n empty properties at position row
        P.insertLine = function(row, n) {
                if (n == null)
                        n = 1;
                if (this.props.length < row) {
                        this.props[row] = null;
                        --n;
                }
                if (n > 0) {
                        var a = new Array(n + 2);
                        a[0] = row;
                        a[1] = 0;
                        this.props.splice.apply(this.props, a);
                }
        };

        // removes n properties at position row
        P.deleteLine = function(row, n) {
                if (n == null)
                        n = 1;
                this.props.splice(row, n);
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
