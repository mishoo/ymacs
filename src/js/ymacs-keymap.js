// This file is part of Ymacs, an extensible source code editor
// (c) Mihai Bazon 2009 <mihai.bazon@gmail.com>
// Distributed under a BSD-style license.
// http://www.ymacs.org/

// @require ymacs-buffer.js

DEFINE_CLASS("Ymacs_Keymap", null, function(D, P){

        var REVERSE_KEYS = {};
        Object.foreach(DlKeyboard, function(val, key) {
                if (typeof val == "number")
                        REVERSE_KEYS[val] = key;
        });

        D.DEFAULT_ARGS = {
                definitions : [ "definitions" , null ],
                buffer      : [ "buffer"      , null ]
        };

        D.FIXARGS = function(args) {
                if (!args.definitions)
                        args.definitions = {};
        };

        D.CONSTRUCT = function() {
                this.defaultHandler = this.makeHandler(this.buffer.COMMANDS["self_insert_command"], "self_insert_command");
                this.currentPrefix = null;
        };

        P.parseKey = function(str) {
                var key = {};
                var a = str.split(/-/);
                a.reverse();
                a.foreach(function(c, i){
                        if (i == 0) {
                                if (typeof DlKeyboard[c] == "number")
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
                if (ev.keyCode in REVERSE_KEYS)
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
                var cmd = func;
                if (func instanceof Array) {
                        args = func.slice(1);
                        func = func[0];
                }
                key = key.split(/\s*&&\s*/);
                if (key.length > 1) {
                        key.foreach(function(key){
                                this.defineKey(key, func, args);
                        }, this);
                } else {
                        if (typeof func == "string")
                                func = this.buffer.COMMANDS[func];
                        key = key[0].trim();
                        var dfn = this.definitions;
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
                        dfn[key.str] = this.makeHandler(func, cmd, args);
                }
        };

        P.defineKeys = function(map) {
                Object.foreach(map, function(func, key){
                        this.defineKey(key, func);
                }, this);
        };

        P.makeHandler = function(func, cmd, args) {
                return this.buffer.makeInteractiveHandler(func, cmd, args);
        };

        P.getHandler = function(keys) {
                var handler = null, def = this.definitions;
                keys.foreach(function(key){
                        var tmp = handler ? handler[key] : def[key];
                        if (tmp) {
                                handler = tmp;
                                if (handler instanceof Function)
                                        $BREAK();
                        }
                        else if (handler) {
                                handler = null;
                                $BREAK();
                        }
                });
                return handler;
        };

});
