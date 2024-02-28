let TEMPLATE = document.createElement("template");

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
};

export { DOM };
