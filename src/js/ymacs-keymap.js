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

// @require ymacs-buffer.js

DEFINE_CLASS("Ymacs_Keymap", null, function(D, P){

    var REVERSE_KEYS = {};
    Object.foreach(DlKeyboard, function(val, key) {
        if (typeof val == "number")
            REVERSE_KEYS[val] = key;
    });

    D.CONSTRUCT = function() {
        this.definitions = Object.makeCopy(this.__originalDefs);
    };

    P.FINISH_OBJECT_DEF = function() {
        if (this.__originalDefs)
            this.__originalDefs = Object.makeCopy(this.__originalDefs);
        else
            this.__originalDefs = {};
        var keys = this.constructor.KEYS;
        if (keys)
            this.defineKeys(keys);
    };

    P.parseKey = function(str) {
        var key = {};
        var a = str.split(/-/);
        a.reverse();
        a.foreach(function(c, i){
            if (i == 0) {
                if (c == "WHEEL_UP" || c == "WHEEL_DOWN") {
                    key.charCode = c;
                }
                else if (typeof DlKeyboard[c] == "number")
                    key.keyCode = DlKeyboard[c];
                else {
                    a[i] = c.toLowerCase();
                    key.charCode = a[i].charCodeAt(0);
                }
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
    };

    D.unparseKey = function(ev) {
        var key, modifiers = [];
        if ("wheelDelta" in ev) {
            key = ev.wheelDelta > 0 ? "WHEEL_UP" : "WHEEL_DOWN";
        }
        else if (ev.keyCode in REVERSE_KEYS)
            key = REVERSE_KEYS[ev.keyCode];
        else if (ev.charCode) {
            if (ev.charCode == 32)
                key = "SPACE";
            else if (ev.charCode == 45)
                key = "DASH";
            else
                key = String.fromCharCode(ev.charCode).toLowerCase();
        }
        if (ev.ctrlKey)
            modifiers.push("C");
        if (ev.altKey)
            modifiers.push("M");
        if (ev.shiftKey && (ev.charCode && /^[a-zA-Z0-9]$/.test(key) || ev.keyCode))
            modifiers.push("S");
        modifiers.sort();
        modifiers = modifiers.join("-");
        if (modifiers)
            modifiers += "-";
        return modifiers + key;
    };

    P.defineKey = function(key, func, args) {
        if (func instanceof Array) {
            args = func.slice(1);
            func = func[0];
        }
        key = key.trim().split(/\s*&&\s*/);
        if (key.length > 1) {
            key.foreach(function(key){
                this.defineKey(key, func, args);
            }, this);
        } else {
            key = key[0].trim();
            var dfn = this.definitions || this.__originalDefs;
            if (key.indexOf(" ") >= 0) {
                var a = key.split(/\s+/);
                key = a.pop();
                a.foreach(function(key){
                    key = this.parseKey(key).str;
                    if (!dfn[key])
                        dfn[key] = {};
                    dfn = dfn[key];
                }, this);
            }
            key = this.parseKey(key);
            dfn[key.str] = [ func, args ];
        }
    };

    P.defineKeys = function(map) {
        Object.foreach(map, function(func, key){
            this.defineKey(key, func);
        }, this);
    };

    P.getHandler = function(keys) {
        var handler = null, def = this.definitions;
        keys.foreach(function(key){
            var tmp = handler ? handler[key] : def[key];
            if (tmp) {
                handler = tmp;
                if (handler instanceof Array)
                    $BREAK();
            }
            else {
                handler = null;
                $BREAK();
            }
        });
        return handler;
    };

    P.attached = Function.noop;
    P.detached = Function.noop;

});
