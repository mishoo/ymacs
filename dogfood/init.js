import { Ymacs } from "../src/index.js";
import "./ymacs-dogfood.js";

window.addEventListener("beforeunload", ev => {
    ev.preventDefault();
    return ev.returnValue = true;
});

let ymacs = window.ymacs = new Ymacs();
Object.assign(ymacs.getElement().style, {
    position : "absolute",
    left     : 0,
    top      : 0,
    width    : "100%",
    height   : "100%"
});

ymacs.setColorTheme("_current");
ymacs.addClass("Ymacs-line-numbers");
ymacs.addClass("Ymacs-hl-line");
// ymacs.delClass("Ymacs-cursor-block");
// ymacs.addClass("Ymacs-cursor-bar");

document.body.appendChild(ymacs.getElement());
ymacs.focus();
