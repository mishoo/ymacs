/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT

let TEMPLATE = document.createElement("template");
let OVERLAY;

class Raw {
    constructor(value) { this._value = value }
    toString() { return this._value }
    valueOf() { return this._value }
}

export let DOM = {
    fromHTML(html) {
        TEMPLATE.innerHTML = html.trim();
        let cont = TEMPLATE.content;
        return cont.children.length > 1 ? cont : cont.children[0];
    },
    addClass(el, cls) {
        el.classList.add(cls);
    },
    delClass(el, cls) {
        if (cls instanceof RegExp) {
            el.className = el.className.replace(cls, "");
        } else {
            el.classList.remove(cls);
        }
    },
    hasClass(el, cls) {
        return el.classList.contains(cls);
    },
    condClass(el, cond, clsTrue, clsFalse) {
        el.classList.toggle(clsTrue, !!cond);
        if (clsFalse) {
            el.classList.toggle(clsFalse, !cond);
        }
    },
    toggleClass(el, ...args) {
        el.classList.toggle(...args);
    },
    trash(el) {
        el && el.remove();
    },
    on(element, event, handler, options) {
        if (typeof event == "string") {
            element.addEventListener(event, handler, options || {});
        } else {
            Object.keys(event).forEach(ev => DOM.on(element, ev, event[ev], handler || {}));
        }
    },
    off(element, event, handler, options) {
        if (typeof event == "string") {
            element.removeEventListener(event, handler, options || {});
        } else {
            Object.keys(event).forEach(ev => DOM.off(element, ev, event[ev], handler || {}));
        }
    },
    overlayOn(cls) {
        document.body.appendChild(OVERLAY);
        if (cls) {
            OVERLAY.className = cls;
        }
        return OVERLAY;
    },
    overlayOff() {
        OVERLAY.remove();
    },
    mousePos(ev, el) {
        let box = el.getBoundingClientRect();
        return {
            x: ev.clientX - box.left,
            y: ev.clientY - box.top
        };
    },
    htmlEscape: function(str) {
        return str instanceof Raw ? (str+"")
            : (str+"").replace(/&/g, "&amp;")
            .replace(/\x22/g, "&quot;")
            .replace(/\x27/g, "&#x27;")
            .replace(/</g, "&lt;")
            .replace(/>/g, "&gt;")
            .replace(/\u00A0/g, "&#xa0;");
    },
    htmlSafe: function(str) {
        return new Raw(str);
    },
};

OVERLAY = DOM.fromHTML(`<div style="position: fixed; z-index: 20000; left: 0; top: 0; right: 0; bottom: 0"></div>`);

export class EventProxy {
    constructor(options) {
        this._ev_handlers = Object.create(null);
        this.o = Object.assign(Object.create(this.constructor.options || null), options);
    }
    addEventListener(event, handler) {
        if (typeof event == "string") {
            let a = this._getHandlers(event);
            remove(a, handler);
            a.push(handler);
        } else {
            Object.keys(event).forEach(ev => this.addEventListener(ev, event[ev]));
        }
    }
    removeEventListener(event, handler) {
        if (typeof event == "string") {
            remove(this._getHandlers(event), handler);
        } else {
            Object.keys(event).forEach(ev => this.removeEventListener(ev, event[ev]));
        }
    }
    listenOnce(event, handler) {
        this.addEventListener(event, function fn(...args) {
            this.removeEventListener(event, fn);
            return handler.apply(this, args);
        });
    }
    callHooks(ev, ...args) {
        this._getHandlers(ev).forEach(f => f.apply(this, args));
    }
    destroy() {
        this.callHooks("onDestroy");
    }
    _getHandlers(ev) {
        return this._ev_handlers[ev] || (this._ev_handlers[ev] = []);
    }
}

export class Widget extends EventProxy {
    constructor(...args) {
        super(...args);
        this.createElement();
    }
    createElement() {
        let el = document.createElement("div");
        el.className = this.initClassName();
        el._ymacs_object = this;
        this.el = el;
        return el;
    }
    initClassName() {
        return this.constructor.name; // XXX: fail after minification
    }
    getContentElement() {
        return this.el;
    }
    getElement() {
        return this.el;
    }
    add(thing) {
        if (thing instanceof Widget) {
            thing = thing.getElement();
        }
        this.getContentElement().appendChild(thing);
    }
    addClass(cls) {
        DOM.addClass(this.getElement(), cls);
    }
    delClass(cls) {
        DOM.delClass(this.getElement(), cls);
    }
    hasClass(cls) {
        return DOM.hasClass(this.getElement(), cls);
    }
    condClass(cond, clsTrue, clsFalse) {
        DOM.condClass(this.getElement(), cond, clsTrue, clsFalse);
    }
    toggleClass(...args) {
        DOM.toggleClass(this.getElement(), ...args);
    }
    setContent(cont) {
        this.getContentElement().innerHTML = cont;
    }
    setStyle(prop, val) {
        let style = this.getElement().style;
        if (typeof prop == "string") {
            style[prop] = val;
        } else {
            Object.assign(style, prop);
        }
    }
    getBox() {
        return this.getElement().getBoundingClientRect();
    }
    destroy() {
        DOM.trash(this.getElement());
        super.destroy(...arguments);
    }
}

export function remove(array, element) {
    let pos = 0;
    while ((pos = array.indexOf(element, pos)) >= 0) {
        array.splice(pos, 1);
    }
}

export function delayed(fn, timeout = 0, obj, ...args) {
    if (arguments.length > 2) {
        fn = fn.bind(obj, ...args);
    }
    let timer = null;
    return function() {
        clearTimeout(timer);
        timer = setTimeout(fn, timeout);
    };
}

var $1K = 1024, $1M = $1K * 1024, $1G = $1M * 1024, $1T = $1G * 1024;
export function formatBytes(number, fixed) {
    var sz = number, spec, r;
    if (sz < $1K) {
        spec = "B";
    } else if (sz < $1M) {
        sz /= $1K;
        spec = "K";
    } else if (sz < $1G) {
        sz /= $1M;
        spec = "M";
    } else if (sz < $1T) {
        sz /= $1G;
        spec = "G";
    }
    // spec = " " + spec;
    r = Math.round(sz);
    if (fixed && sz != r)
        return sz.toFixed(fixed) + spec;
    else
        return r + spec;
}

export function zeroPad(thing, width, zero = "0") {
    if (typeof thing == "number") {
        thing = Math.round(thing);
    }
    var s = "" + thing;
    while (s.length < width)
        s = zero + s;
    return s;
}

export function common_prefix(strings) {
    switch (strings.length) {
      case 0:
        return "";
      case 1:
        return strings[0];
      case 2:
        let a = strings[0];
        let b = strings[1];
        let n = Math.min(a.length, b.length);
        let i = 0;
        while (i < n && a.charAt(i) === b.charAt(i)) ++i;
        return a.substring(0, i);
      default:
        return common_prefix([ strings[0], common_prefix(strings.slice(1)) ]);
    }
}

export function getYmacsThemes() {
    let themes = [];
    for (let st of document.styleSheets) digStyle(st);
    return themes;

    function digStyle(st) {
        for (let rule of st.cssRules) digRule(rule);
    }

    function digRule(rule) {
        if (rule instanceof CSSImportRule) {
            digStyle(rule.styleSheet);
        } else {
            let m = /\.Ymacs-Theme-([\p{L}0-9_-]+)/u.exec(rule.selectorText);
            if (m) {
                if (themes.indexOf(m[1]) < 0) {
                    themes.push(m[1]);
                }
            }
        }
    }
}

function fuzzy_regexp(query) {
    return new RegExp([...query].map(ch => {
        ch = ch.replace(/[\]\[\}\{\)\(\*\+\?\.\\\^\$\|]/ug, "\\$&")
            .replace(/[\s_-]/ug, "[\\s_-]");
        return `(${ch})(.*?)`;
    }).join(""), "guid");
}

export function fuzzy_filter(candidates, query) {
    query = query.trim();
    if (!query) return candidates;
    let re = fuzzy_regexp(query);
    let results = [];
    candidates.forEach(item => {
        re.lastIndex = 0;
        while (true) {
            let m = re.exec(item);
            if (!m) break;
            let score = m.index;
            let hil = "";
            let j = 0;
            for (let i = 1; i < m.indices.length;) {
                let [ li_beg, li_end ] = m.indices[i++];
                let [ fi_beg, fi_end ] = m.indices[i++];
                score += 10 * (fi_end - fi_beg);
                hil += DOM.htmlEscape(item.substring(j, li_beg))
                    + `<b>${item.substring(li_beg, li_end)}</b>`;
                j = li_end;
            }
            if (j != null) hil += DOM.htmlEscape(item.substr(j));
            results.push({ label: DOM.htmlSafe(hil), value: item, score: score });
            re.lastIndex = m.index + 1;
        }
    });
    return results.sort((a, b) => a.score - b.score).reduce((a, item) => {
        if (!a.some(el => el.value == item.value)) a.push(item);
        return a;
    }, []);
}
