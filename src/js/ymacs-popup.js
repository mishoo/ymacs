/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT

import { DOM, Widget } from "./ymacs-utils.js";
import { Ymacs_Keymap } from "./ymacs-keymap.js";
import { Ymacs_Buffer } from "./ymacs-buffer.js";

let MENU = Symbol("MENU");

export class Ymacs_Popup extends Widget {
    static options = {};

    constructor(...args) {
        super(...args);
        this._cont = DOM.fromHTML(`<div class="Ymacs_Menu"></div>`);
        this.getElement().appendChild(this._cont);
    }

    getContentElement() {
        return this._cont;
    }

    initClassName() {
        return "Ymacs_Popup";
    }

}

var KEYMAP_MENU_ACTIVE = Ymacs_Keymap.define(null, {
    "ArrowDown && C-n" : handle_arrow_down,
    "ArrowUp && C-p"   : handle_arrow_up,
    "PageDown"         : handle_arrow_down,
    "PageUp"           : handle_arrow_up,
    "C-End && M->"     : handle_popup_end,
    "C-Home && M-<"    : handle_popup_home,
    "Enter"            : handle_enter,
    "Escape"           : handle_escape,
});

KEYMAP_MENU_ACTIVE.defaultHandler = [ function() {
    let buffer = this;
    let menu = buffer[MENU];
    menu.kill();
    return false; // say it's not handled though
} ];

export function popupMenu({
    buffer,
    items,
    onSelect,
} = {}) {
    let menuWidget = new Ymacs_Popup();
    menuWidget.addClass("with-arrow");
    items.forEach((label, index) => {
        let value = label;
        if (typeof label != "string") {
            value = label.value;
            label = label.label;
        }
        let el = DOM.fromHTML(`<div class="Ymacs_Menu_Item" data-value="${DOM.htmlEscape(value)}"
                                          data-index="${index}">${DOM.htmlEscape(label)}</div>`);
        menuWidget.add(el);
    });

    buffer[MENU] = {
        items: items,
        widget: menuWidget,
        onSelect: onSelect,
        kill() {
            menuWidget.destroy();
            buffer[MENU] = null;
        }
    };

    let activeElement = document.activeElement;
    let ymacs = buffer.ymacs;
    DOM.on(menuWidget.getContentElement(), {
        click: ev => {
            activeElement.focus();
            let item = ev.target;
            if (!DOM.hasClass(item, "Ymacs_Menu_Item")) return;
            select(buffer, +item.dataset.index);
            handle_enter.call(buffer);
        },
    });
    ymacs._popupAtCaret(menuWidget.getElement());
    select(buffer, 0);

    buffer.pushKeymap(KEYMAP_MENU_ACTIVE);
    menuWidget.addEventListener("onDestroy", () => {
        buffer.popKeymap(KEYMAP_MENU_ACTIVE);
    });
}

function handle_arrow_up() {
    let buffer = this;
    let menu = buffer[MENU];
    select(buffer, menu.selectedIndex - 1);
}

function handle_arrow_down() {
    let buffer = this;
    let menu = buffer[MENU];
    select(buffer, menu.selectedIndex + 1);
}

function handle_popup_home() {
    let buffer = this;
    let menu = buffer[MENU];
    select(buffer, 0);
}

function handle_popup_end() {
    let buffer = this;
    let menu = buffer[MENU];
    select(buffer, -1);
}

function handle_enter() {
    let buffer = this;
    let menu = buffer[MENU];
    if (!(menu.onSelect(menu.selectedIndex) === false)) {
        menu.kill();
    }
}

function handle_escape() {
    let buffer = this;
    let menu = buffer[MENU];
    menu.kill();
}

function select(buffer, index) {
    let menu = buffer[MENU];
    let cont = menu.widget.getContentElement();
    let elements = [...cont.querySelectorAll(".Ymacs_Menu_Item")];
    let n = elements.length;
    index = menu.selectedIndex = ((index % n) + n) % n;
    elements.forEach(el => {
        let current = el.dataset.index == index;
        if (current) menu.selectedItem = el;
        DOM.condClass(el, current, "selected");
    });
    menu.selectedItem.scrollIntoView({ block: "nearest" });
}

Ymacs_Buffer.newCommands({
    popup_menu: function(args) {
        args.buffer = this;
        popupMenu(args);
    }
});
