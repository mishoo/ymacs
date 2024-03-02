let TEMPLATE = document.createElement("template");
let OVERLAY;

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
        el.classList.remove(cls);
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
            x: ev.pageX - box.left,
            y: ev.pageY - box.top
        };
    },
    htmlEscape: function(str) {
        return str.replace(/&/g, "&amp;")
            .replace(/\x22/g, "&quot;")
            .replace(/\x27/g, "&#x27;")
            .replace(/</g, "&lt;")
            .replace(/>/g, "&gt;")
            .replace(/\u00A0/g, "&#xa0;");
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
        let fn = (...args) => {
            this.removeEventListener(event, fn);
            handler.apply(this, args);
        };
        this.addEventListener(event, fn);
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
    add(widget) {
        this.getContentElement().appendChild(widget.getElement());
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
