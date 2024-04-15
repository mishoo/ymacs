/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT

import { DOM, EventProxy } from "./ymacs-utils.js";

export class Ymacs_Text_Properties extends EventProxy {

    constructor({ buffer }) {
        super(...arguments);
        this.buffer = buffer;
        this.reset();
    }

    reset() {
        this.props = [];
    }

    insertLine(row) {
        if (this.props.length < row)
            this.props[row] = null;
        else {
            this.props.splice(row, 0, null);
        }
    }

    deleteLine(row) {
        this.props.splice(row, 1);
    }

    replaceLine(row, text) {
        var p = this.props[row];
        if (p && p.length > text.length) {
            // remove extra-properties
            p.splice(text.length);
        }
    }

    addLineProps(row, i, j, prop, val) {
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
    }

    removeLineProps(row, i, j, prop) {
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
    }

    spliceLineProps(row, i, diff) {
        let p = this.props[row];
        if (p) {
            if (diff > 0) {
                p.splice(i, 0, ...new Array(diff));
            } else if (diff < 0) {
                p.splice(i, -diff);
            }
        }
    }

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
    getLineHTML(row, text, caret) {
        var p = this.props[row];
        if (caret === null) {
            if (text == "")
                return "<br/>";
            if (!p || p.length == 0) {
                return DOM.htmlEscape(text);
            }
        } else {
            if (text == "")
                return "<span class='Ymacs-caret'>&nbsp;</span>";
            if (!p || p.length == 0) {
                if (caret === text.length)
                    return DOM.htmlEscape(text) + "<span class='Ymacs-caret'>&nbsp;</span>";
                return DOM.htmlEscape(text.substr(0, caret)) +
                    "<span class='Ymacs-caret'>" +
                    DOM.htmlEscape(text.charAt(caret)) +
                    "</span>" +
                    DOM.htmlEscape(text.substr(caret + 1));
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
    }

}
