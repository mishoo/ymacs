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
    }
};

OVERLAY = DOM.fromHTML(`<div style="position: fixed; z-index: 20000; left: 0; top: 0; right: 0; bottom: 0"></div>`);

export class EventProxy {
    constructor(options) {
        this._ev_handlers = Object.create(null);
        this.o = Object.assign(Object.create(this.constructor.options || null), options);
    }
    addEventListener(event, handler) {
        if (typeof event == "string") {
            this._getHandlers(event).push(handler);
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
}

export function remove(array, element) {
    let pos = 0;
    while ((pos = array.indexOf(element, pos)) >= 0) {
        array.splice(pos, 1);
    }
}
