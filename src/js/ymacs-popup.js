/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT

import { DOM, Widget } from "./ymacs-utils.js";

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
