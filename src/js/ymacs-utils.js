let TEMPLATE = document.createElement("template");
let OVERLAY;

let DOM = {
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

OVERLAY = DOM.fromHTML(`<div style="position: fixed; z-index: 20000; left: 0; top: 0; right: 0; bottom: 0">`);

class Widget {
    constructor(options) {
        this.o = Object.assign(Object.create(this.constructor.options), options);
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
        return this.constructor.name; // XXX: probabil fail after minification
    }
    getContentElement() {
        return this.el;
    }
    getElement() {
        return this.el;
    }
    add(wid) {
        this.getContentElement().appendChild(wid.getElement());
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

export { DOM, Widget };
