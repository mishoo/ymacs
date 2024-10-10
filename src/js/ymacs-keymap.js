/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT

let KEYMAPS = Object.create(null);

export class Ymacs_Keymap {

    constructor(keys) {
        this.definitions = Object.create(null);
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

    static parseKey(orig) {
        let key = { str: "" };
        let p = orig;
        for (;;) {
            let m = /^([CMS])-/.exec(p);
            if (m) {
                if (m[1] == "C") key.ctrlKey = true;
                if (m[1] == "M") key.metaKey = true;
                if (m[1] == "S") key.shiftKey = true;
                p = p.substr(2);
            } else {
                key.key = p == "Space" ? " "
                    : p.length == 1 ? p.toLowerCase()
                    : p;
                break;
            }
        }
        if (key.ctrlKey) key.str += "C-";
        if (key.metaKey) key.str += "M-";
        if (key.shiftKey) key.str += "S-";
        key.str += p;
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
        if (ev.altKey || ev.ymacsMeta)
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
            var dfn = this.definitions;
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
