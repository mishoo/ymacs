import { DOM, Widget } from "./ymacs-utils.js";

export class Ymacs_Popup extends Widget {
    static options = {};

    constructor(...args) {
        super(...args);
    }

    initClassName() {
        return "Ymacs_Popup";
    }

}
