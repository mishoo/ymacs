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
