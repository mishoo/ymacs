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

import "./ymacs-buffer.js";

let KEYMAPS = Object.create(null);

export class Ymacs_Keymap {

    constructor(keys) {
        this.defineKeys(keys);
    }

    static define(name, keys) {
        let obj = name && KEYMAPS[name];
        if (!obj) {
            obj = keys instanceof this ? keys : new this(keys);
            if (name) {
                KEYMAPS[name] = obj;
            }
        }
        return obj;
    }

    static get(name) {
        return KEYMAPS[name];
    }

    static parseKey(str) {
        var key = {};
        var a = str.split(/-/);
        a.reverse();
        a.forEach((c, i) => {
            if (i == 0) {
                key.key = c == "Space" ? " " : c.length == 1 ? c.toLowerCase() : c;
            } else switch(c) {
                case "C": key.ctrlKey = true; break;
                case "M": key.metaKey = true; break;
                case "S": key.shiftKey = true; break;
            }
        });
        a.reverse();
        var c = a.pop();
        key.str = a.sort().join("-");
        if (key.str)
            key.str += "-";
        key.str += c;
        return key;
    }

    static unparseKey(ev) {
        var key, a = [];
        if ("wheelDelta" in ev) {
            key = ev.wheelDelta > 0 ? "WheelUp" : "WheelDown";
        } else {
            key = ev.key;
            if (key == " ") key = "Space";
            if (key.length == 1) key = key.toLowerCase();
        }
        if (ev.ctrlKey)
            a.push("C");
        if (ev.altKey)
            a.push("M");
        if (ev.shiftKey && (key.length > 1 || key.toLowerCase() != key.toUpperCase()))
            a.push("S");
        a.sort();
        a.push(key);
        return a.join("-");
    }

    defineKey(key, func, args) {
        if (func instanceof Array) {
            args = func.slice(1);
            func = func[0];
        }
        key = key.trim().split(/\s*&&\s*/);
        if (key.length > 1) {
            key.forEach(key => this.defineKey(key, func, args));
        } else {
            key = key[0].trim();
            var dfn = this.definitions || (this.definitions = Object.create(null));
            if (key.indexOf(" ") >= 0) {
                var a = key.split(/\s+/);
                key = a.pop();
                a.forEach(key => {
                    key = this.parseKey(key).str;
                    if (!dfn[key])
                        dfn[key] = {};
                    dfn = dfn[key];
                });
            }
            key = this.parseKey(key);
            dfn[key.str] = [ func, args ];
        }
    }

    defineKeys(map) {
        Object.keys(map).forEach(key => this.defineKey(key, map[key]));
    }

    getHandler(keys) {
        let handler = null, def = this.definitions;
        for (let key of keys) {
            let tmp = handler ? handler[key] : def[key];
            if (tmp) {
                handler = tmp;
                if (Array.isArray(handler)) {
                    break;
                }
            } else {
                handler = null;
                break;
            }
        }
        return handler;
    }

    attached(){}
    detached(){}

}

Ymacs_Keymap.prototype.parseKey = Ymacs_Keymap.parseKey;

window.Ymacs_Keymap = Ymacs_Keymap; // XXX.
