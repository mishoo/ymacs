/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT

import { DOM, Widget, delayed } from "./ymacs-utils.js";

function textColX(div, col) {
    let range = document.createRange();
    range.selectNodeContents(div);
    let treeWalker = document.createTreeWalker(div, NodeFilter.SHOW_TEXT);
    let len = 0;
    while (treeWalker.nextNode()) {
        let node = treeWalker.currentNode;
        let clen = node.nodeValue.length;
        if (len + clen >= col) {
            range.setStart(node, col - len);
            range.setEnd(node, col - len);
            break;
        }
        len += clen;
    }
    return range.getBoundingClientRect().right;
}

function normalizeRect(p) {
    return p.line1 > p.line2 || (p.line1 == p.line2 && p.col1 > p.col2) ?
        { line1: p.line2, col1: p.col2, line2: p.line1, col2: p.col1 } : p;
}

var DBL_CLICK_SPEED = 300;
var CLICK_COUNT = 0, CLICK_COUNT_TIMER = null, CLICK_LAST_TIME = null;
function CLEAR_CLICK_COUNT() { CLICK_COUNT = null };

var LINE_DIV = DOM.fromHTML(`<div class="line"><br/></div>`);

var COUNT = 0;

export class Ymacs_Frame extends Widget {

    static options = {
        highlightCurrentLine : true,
        buffer               : null,
        ymacs                : null,
        isMinibuffer         : false,
    };

    constructor(...args) {
        super(...args);

        this.buffer = this.o.buffer;
        this.ymacs = this.o.ymacs;
        this.isMinibuffer = this.o.isMinibuffer;

        // <XXX> // during transition
        this.el = this.getElement();
        this.el._ymacs_object = this;
        // </XXX>

        this.id = `ymacs-frame-${++COUNT}`;
        this.__caretId = `ymacs-caret-${COUNT}`;
        this.redrawModelineWithTimer = delayed(this.redrawModeline.bind(this));

        this.getElement().tabIndex = 0;
        this.getElement().innerHTML = "<div class='Ymacs-frame-overlays'>"
            + "<div class='Ymacs-frame-content'></div>"
            + "</div>"
            + "<div class='Ymacs_Modeline'></div>";

        this.addEventListener({
            onDestroy    : this._on_destroy,
        });

        this._dragSelectHandlers = {
            mousemove : this._dragSelect_onMouseMove.bind(this),
            mouseup   : this._dragSelect_onMouseUp.bind(this)
        };

        this._bufferEvents = {
            onLineChange             : this._on_bufferLineChange.bind(this),
            onInsertLine             : this._on_bufferInsertLine.bind(this),
            onDeleteLine             : this._on_bufferDeleteLine.bind(this),
            onPointChange            : this._on_bufferPointChange.bind(this),
            onResetCode              : this._on_bufferResetCode.bind(this),
            onOverwriteMode          : this._on_bufferOverwriteMode.bind(this),
            onProgressChange         : this._on_bufferProgressChange.bind(this),
            beforeInteractiveCommand : this._on_bufferBeforeInteractiveCommand.bind(this),
            afterInteractiveCommand  : this._on_bufferAfterInteractiveCommand.bind(this),
            onOverlayChange          : this._on_bufferOverlayChange.bind(this),
            onOverlayDelete          : this._on_bufferOverlayDelete.bind(this),
        };

        this._moreBufferEvents = {
            onMessage               : this._on_bufferMessage.bind(this),
            afterInteractiveCommand : function(){
                if (this.__ensureCaretVisible)
                    this.ensureCaretVisible();
            }.bind(this)
        };

        var buffer = this.buffer;
        this.buffer = null;
        if (buffer)
            this.setBuffer(buffer);

        DOM.on(this.getOverlaysContainer(), "scroll", this._on_scroll.bind(this));
        DOM.on(this.getElement(), {
            mousedown   : this._dragSelect_onMouseDown.bind(this),
            focus       : this._on_focus.bind(this),
            blur        : this._on_blur.bind(this),
            keydown     : this._on_keyDown.bind(this),
            keyup       : this._on_keyUp.bind(this),
            wheel       : this._on_mouseWheel.bind(this),
        });
    }

    initClassName() {
        return `Ymacs_Frame${this.o.isMinibuffer ? ' Ymacs_Minibuffer' : ''}`;
    }

    focus(exitAllowed) {
        this.getElement().focus();
        // if (exitAllowed instanceof Function) {
        //     this.removeEventListener("onBlur", this.__exitFocusHandler);
        //     this.addEventListener("onBlur", this.__exitFocusHandler = function(){
        //         if (exitAllowed.call(this.buffer)) {
        //             this.removeEventListener("onBlur", this.__exitFocusHandler);
        //         } else {
        //             this.focus.delayed(2, this, null);
        //         }
        //     });
        // }
    }

    blur(force) {
        // if (force)
        //     this.removeEventListener("onBlur", this.__exitFocusHandler);
    }

    getOverlaysContainer() {
        return this.getElement().firstChild;
    }

    getModelineElement() {
        return this.getElement().childNodes[1];
    }

    getContentElement() {
        return this.getElement().firstChild.firstChild;
    }

    getCaretElement() {
        return document.getElementById(this.__caretId);
    }

    getLineDivElement(row) {
        return this.getContentElement().childNodes[row] || null;
    }

    ensureCaretVisible() {
        // return true if the scroll position has changed
        let div = this.getOverlaysContainer();
        let st = div.scrollTop;
        this.redrawCaret();
        let caret = this.getCaretElement();
        if (caret) {
            caret.scrollIntoView({ block: "nearest", inline: "nearest" });
            if (caret.offsetLeft < div.clientWidth / 2) {
                div.scrollLeft = 0;
            }
        }
        return st != div.scrollTop;
    }

    focusInside() {
        return document.activeElement === this.getElement();
    }

    setBuffer(buffer) {
        if (this.buffer) {
            if (this.caretMarker && !this.o.isMinibuffer) {
                this.caretMarker.destroy();
                this.caretMarker = null;
            }
            this.buffer.removeEventListener(this._bufferEvents);
            this.buffer.removeEventListener(this._moreBufferEvents);
        }
        this.buffer = buffer;
        if (buffer) {
            buffer.addEventListener(this._bufferEvents);
            if (this.focusInside()) {
                buffer.addEventListener(this._moreBufferEvents);
            }
            if (this.o.isMinibuffer) {
                this.caretMarker = buffer.caretMarker;
            } else {
                this.caretMarker = buffer.createMarker(buffer.caretMarker.getPosition(), false, "framecaret");
            }
            this._redrawBuffer();
            this.redrawCaret(true);
            this.centerOnCaret();
        }
    }

    recenterTopBottom() {
        let row = this.buffer._rowcol.row;
        let line = this.getLineDivElement(row);
        let div = this.getOverlaysContainer();
        let st = div.scrollTop;
        let center = Math.round(line.offsetTop - div.clientHeight / 2 + line.offsetHeight / 2);
        let top = Math.round(line.offsetTop) - 1;
        let bottom = Math.round(line.offsetTop - div.clientHeight + line.offsetHeight) + 1;
        if (st == top) div.scrollTop = bottom;
        else if (st == center) div.scrollTop = top;
        else div.scrollTop = center;
    }

    centerOnCaret() {
        this.centerOnLine(this.buffer._rowcol.row);
    }

    centerOnLine(row) {
        var line = this.getLineDivElement(row), div = this.getOverlaysContainer();
        div.scrollTop = Math.round(line.offsetTop - div.clientHeight / 2 + line.offsetHeight / 2);
        // this._redrawBuffer();
    }

    setModelineContent(html) {
        this.getModelineElement().innerHTML = html;
    }

    deleteOtherFrames() {
        this.ymacs.keepOnlyFrame(this);
    }

    deleteFrame() {
        this.ymacs.deleteFrame(this);
    }

    _split(horiz) {
        let ovdiv = this.getOverlaysContainer();
        let scroll = ovdiv.scrollTop;
        let fr = this.ymacs.createFrame({ buffer: this.buffer });
        let sc = new Ymacs_SplitCont({ horiz: horiz });
        this.getElement().replaceWith(sc.getElement());
        sc.setSplit(this, fr);
        ovdiv.scrollTop = scroll;
        fr.getOverlaysContainer().scrollTop = scroll;
        this.focus(); // XXX: only if necessary
        return fr;
    }

    vsplit() {
        return this._split(true);
    }

    hsplit(percent) {
        return this._split(false);
    }

    __showCaret() {
        DOM.addClass(this.getCaretElement(), "Ymacs-caret");
    }

    _unhoverLine() {
        let el = this.__hoverLine != null && this.getLineDivElement(this.__hoverLine);
        if (el) DOM.delClass(el, "Ymacs-current-line");
        this.__hoverLine = null;
    }

    redrawCaret(force) {
        if (this.o.isMinibuffer) force = true;
        var isActive = this.ymacs.getActiveFrame() === this;
        if (!force && !isActive)
            return;

        if (isActive && !this.o.isMinibuffer && this.focusInside())
            this.caretMarker.setPosition(this.buffer.caretMarker.getPosition());

        var rc = this.buffer._rowcol;

        if (this.o.highlightCurrentLine) {
            this._unhoverLine();
            DOM.addClass(this.getLineDivElement(rc.row), "Ymacs-current-line");
            this.__hoverLine = rc.row;
        }

        // hide stale carets :-\
        // mess everywhere.
        [...this.getElement().querySelectorAll(".Ymacs-caret, #" + this.__caretId)].forEach(el => {
            el.id = "";
            el.className = "";
        });

        // redraw the line where the caret was previously, so that it disappears from there
        if (this.__prevCaretLine != null) {
            this._on_bufferLineChange(this.__prevCaretLine);
        }

        // redraw current line if it's different
        if (this.__prevCaretLine != rc.row) {
            this.__prevCaretLine = rc.row;
            this._on_bufferLineChange(rc.row);
        }

        this.callHooks("onPointChange", rc.row, rc.col);
        this.redrawModelineWithTimer(rc);
    }

    _getLineHTML(row) {
        var html = this.buffer.formatLineHTML(row, this.caretMarker);
        // taking advantage of the fact that a literal > entered by the user will never appear in
        // the generated HTML, since special HTMl characters are escaped.
        var pos = html.indexOf("Ymacs-caret'>");
        if (pos >= 0) {
            html = html.substr(0, pos + 12)
                + " id='" + this.__caretId + "'"
                + html.substr(pos + 12);
        }
        return html;
    }

    _redrawBuffer() {
        this.setContent(this.buffer.code.map((line, i) =>
            `<div class="line">${this._getLineHTML(i)}</div>`
        ).join(""));
    }

    coordinatesToRowCol(x, y) {
        function findLine(r1, r2) {
            if (r1 >= r2)
                return r1;
            var row = Math.floor((r1 + r2) / 2);
            var div = self.getLineDivElement(row);
            var y1  = div.offsetTop;
            var y2  = y1 + div.offsetHeight;
            if (y2 < y)
                return findLine(row + 1, r2);
            if (y < y1)
                return findLine(r1, row - 1);
            return row;
        };
        function findCol(c1, c2) {
            if (c1 >= c2)
                return c1;
            var col = Math.floor((c1 + c2) / 2);
            var p1 = self.coordinates(row, col),
                p2 = self.coordinates(row, col + 1);
            if (p2.x < x)
                return findCol(col + 1, c2);
            if (x < p1.x)
                return findCol(c1, col - 1);
            return col;
        };
        var self = this,
            row = findLine(0, this.buffer.code.length - 1),
            col = findCol(0, this.buffer.code[row].length);
        return { row: row, col: col };
    }

    coordinates(row, col) {
        var box = this.getContentElement().getBoundingClientRect();
        var div = this.getLineDivElement(row);
        return {
            x: textColX(div, col) - box.left,
            y: div.offsetTop,
            h: div.offsetHeight
        };
    }

    heightInLines() {
        return Math.floor(this.getOverlaysContainer().clientHeight / this.getContentElement().firstChild.offsetHeight);
    }

    redrawModeline(rc) {
        if (!rc) rc = this.caretMarker.getRowCol();
        var maxline = this.buffer.code.length - 1;
        var firstline = this.firstLineVisible();
        var lastline = this.lastLineVisible();
        var percent = firstline == 0
            ? "Top"
            : lastline == maxline
            ? "Bot"
            : Math.round(lastline / maxline * 100) + "%";
        this.setModelineContent(this.buffer.renderModelineContent(rc, percent));
    }

    /* -----[ event handlers ]----- */

    _on_bufferLineChange(row) {
        var div = this.getLineDivElement(row);
        if (div) {
            //console.log("Redrawing line %d [%s]", row, this.buffer.code[row]);
            //console.log(new Error().stack);
            div.innerHTML = this._getLineHTML(row);
        }
    }

    _on_bufferInsertLine(row, drawIt) {
        var div = LINE_DIV.cloneNode(true);
        this.getContentElement().insertBefore(div, this.getLineDivElement(row));
        if (drawIt) {
            div.innerHTML = this._getLineHTML(row);
        }
    }

    _on_bufferDeleteLine(row) {
        DOM.trash(this.getLineDivElement(row));
    }

    _on_bufferPointChange(rc, pos) {
        this.redrawCaret();
    }

    _on_bufferResetCode() {
        this._redrawBuffer();
    }

    _on_bufferOverwriteMode(om) {
        this.condClass(om, "Ymacs-overwrite-mode");
    }

    _on_bufferMessage(args) {
        this.ymacs.popupMessage(args);
    }

    _on_bufferBeforeInteractiveCommand() {
        this.__ensureCaretVisible = true;
        this._unhoverLine();
        this.ymacs.clearPopupMessage();
    }

    _on_bufferAfterInteractiveCommand() {}

    _on_bufferProgressChange() {
        this.redrawModelineWithTimer(null);
    }

    getOverlayId(name) {
        return this.id + "-ovl-" + name;
    }

    getOverlayHTML(name, props) {
        var str = "";
        props.forEach(p => {
            if (p.line1 == p.line2 && p.col1 == p.col2) {
                return;
            }
            p = normalizeRect(p);
            var p1 = this.coordinates(p.line1, p.col1);
            var p2 = this.coordinates(p.line2, p.col2);
            var p0 = p.col1 == 0 ? p1 : this.coordinates(p.line1, 0);
            if (p.line1 == p.line2) {
                str += `<div class="${name}" style="
                    top: ${p1.y}px;
                    left: ${p1.x}px;
                    height: ${p1.h}px;
                    width: ${p2.x - p1.x}px;
                "></div>`;
            } else {
                str += `<div class="${name}" style="
                    top: ${p1.y}px;
                    left: ${p1.x}px;
                    height: ${p.col1 > 0 ? p1.h : p2.y - p1.y}px;
                "></div>`;
                if (p.col1 > 0 && p.line2 - p.line1 > 1) {
                    str += `<div class="${name}" style="
                        top: ${p1.y + p1.h}px;
                        left: ${p0.x}px;
                        height: ${p2.y - p1.y - p1.h}px;
                    "></div>`;
                }
                if (p.col2 > 0) {
                    str += `<div class="${name}" style="
                        top: ${p2.y}px;
                        left: ${p0.x}px;
                        height: ${p2.h}px;
                        width: ${p2.x - p0.x}px;
                    "></div>`;
                }
            }
        });
        return str ? `<div id="${this.getOverlayId(name)}" data-ymacs-overlay="${name}" class="Ymacs_Overlay ${name}">${str}</div>` : null;
    }

    getOverlaysCount() {
        return this.getOverlaysContainer().childNodes.length - 1; // XXX: subtract the div.content; we need to revisit this if we add new elements.
    }

    _on_bufferOverlayChange(name, props) {
        let html = this.getOverlayHTML(name, Array.isArray(props) ? props : [ props ]);
        if (html) {
            let div = DOM.fromHTML(html);
            let old = document.getElementById(this.getOverlayId(name));
            old ? old.replaceWith(div) : this.getOverlaysContainer().appendChild(div);
            this._setOverlayClasses();
        } else {
            this._on_bufferOverlayDelete(name);
        }
    }

    _on_bufferOverlayDelete(name) {
        DOM.trash(document.getElementById(this.getOverlayId(name)));
        this._setOverlayClasses();
    }

    _setOverlayClasses() {
        let names = [
            ...this.getOverlaysContainer().querySelectorAll(":scope > [data-ymacs-overlay]")
        ].map(el => el.dataset.ymacsOverlay);
        this.condClass(names.length > 0, "Ymacs_Frame-hasOverlays");
        this.getElement().dataset.ymacsOverlays = names.join(" ");
    }

    /* -----[ self events ]----- */

    _on_destroy() {
        this.setBuffer(null);
    }

    _on_focus() {
        this.ymacs.setActiveFrame(this, true);
        if (!this.o.isMinibuffer) {
            this.buffer.cmd("goto_char", this.caretMarker.getPosition());
        }
        this.buffer.addEventListener(this._moreBufferEvents);
        this.redrawCaret();
    }

    _on_blur() {
        if (!this.o.isMinibuffer) {
            this.caretMarker.setPosition(this.buffer.caretMarker.getPosition());
        }
        this.buffer.removeEventListener(this._moreBufferEvents);
    }

    _dragSelect_onMouseDown(ev) {
        if (ev.ctrlKey && ev.shiftKey)
            return;
        ev.stopPropagation();

        clearTimeout(CLICK_COUNT_TIMER);
        CLICK_COUNT++;
        CLICK_COUNT_TIMER = setTimeout(CLEAR_CLICK_COUNT, DBL_CLICK_SPEED);

        let pos = DOM.mousePos(ev, this.getContentElement());
        let rc = this.coordinatesToRowCol(pos.x, pos.y);
        let buf = this.buffer;

        setTimeout(() => {
            buf.clearTransientMark();
            buf.cmd("goto_char", buf._rowColToPosition(rc.row, rc.col));
            buf.callInteractively("keyboard_quit");
            if (CLICK_COUNT == 1) {
                buf.ensureTransientMark();
                DOM.on(window, this._dragSelectHandlers);
            }
            else if (CLICK_COUNT == 2) {
                buf.cmd("forward_word");
                buf.cmd("backward_word");
                buf.cmd("forward_word_mark");
            }
            else if (CLICK_COUNT == 3) {
                buf.cmd("beginning_of_line");
                buf.cmd("end_of_line_mark");
            }
            else if (CLICK_COUNT == 4) {
                buf.cmd("backward_paragraph");
                buf.cmd("forward_whitespace");
                buf.cmd("beginning_of_line");
                buf.cmd("forward_paragraph_mark");
            }
        });
    }

    _dragSelect_onMouseMove(ev) {
        let pos = DOM.mousePos(ev, this.getContentElement());
        let rc = this.coordinatesToRowCol(pos.x, pos.y);
        this.buffer.cmd("goto_char", this.buffer._rowColToPosition(rc.row, rc.col));
        this.buffer.ensureTransientMark();
        this.ensureCaretVisible();
    }

    _dragSelect_onMouseUp(ev) {
        DOM.off(window, this._dragSelectHandlers);
    }

    _on_keyDown(ev) {
        if (!isModifier(ev.key)) {
            if (this.ymacs.processKeyEvent(ev)) {
                ev.preventDefault();
                ev.stopPropagation();
            }
        }
    }

    _on_keyUp(ev) {
    }

    _on_scroll() {
        this.redrawModelineWithTimer();
    }

    _on_mouseWheel(ev) {
        ev.preventDefault();
        this.buffer._handleKeyEvent(ev);
    }

    firstLineVisible() {
        var div = this.getOverlaysContainer();
        return this.coordinatesToRowCol(1, div.scrollTop + 1).row;
    }

    lastLineVisible() {
        var div = this.getOverlaysContainer();
        return this.coordinatesToRowCol(div.clientWidth - 2, div.scrollTop + div.clientHeight - 2).row;
    }

    scrollUp(lines) {
        var div = this.getOverlaysContainer();
        var line = Math.max(this.firstLineVisible() - lines, 0);
        line = this.getLineDivElement(line);
        div.scrollTop = line.offsetTop;
        this.__ensureCaretVisible = false;
    }

    scrollDown(lines) {
        var div = this.getOverlaysContainer();
        var line = Math.min(this.firstLineVisible() + lines, this.buffer.code.length - 1);
        line = this.getLineDivElement(line);
        div.scrollTop = line.offsetTop;
        this.__ensureCaretVisible = false;
    }
}

function isModifier(key) {
    return /^(?:Alt|AltGraph|CapsLock|Control|Fn|FnLock|Hyper|Meta|NumLock|ScrollLock|Shift|Super|Symbol|SymbolLock)$/.test(key);
}

export class Ymacs_SplitCont extends Widget {
    static options = {
        horiz: false
    };
    #activeElement = null;
    initClassName() {
        return "Ymacs_SplitCont " + (this.o.horiz ? "horiz" : "vert");
    }
    createElement() {
        super.createElement();
        this._dragHandlers = {
            mousemove : this._onMouseMove.bind(this),
            mouseup   : this._onMouseUp.bind(this),
        };
        this._rb = DOM.fromHTML(`<div class="bar" tabindex="0"></div>`);
        DOM.on(this._rb, "mousedown", this._onMouseDown.bind(this));
    }
    setSplit(a, b) {
        this.add(a);
        this.add(this._rb);
        this.add(b);
    }
    _onMouseDown(ev) {
        this.#activeElement = document.activeElement;
        if (ev.target === this._rb) {
            let first = this.getContentElement().children[0];
            this._start = this.o.horiz ? ev.clientY : ev.clientX;
            this._orig_sz = this.o.horiz ? first.offsetHeight : first.offsetWidth;
            this.addClass("dragging");
            ev.stopPropagation();
            DOM.on(window, this._dragHandlers);
            DOM.overlayOn(this.o.horiz ? "Ymacs_Resize_horiz" : "Ymacs_Resize_vert");
        } else if (this.#activeElement) {
            this.#activeElement.focus();
        }
    }
    _onMouseUp(ev) {
        DOM.off(window, this._dragHandlers);
        this.delClass("dragging");
        DOM.overlayOff();
        if (this.#activeElement) {
            this.#activeElement.focus();
        }
        this.#activeElement = null;
    }
    _onMouseMove(ev) {
        let cont = this.getElement();
        let max = this.o.horiz ? cont.offsetHeight : cont.offsetWidth;
        let diff = (this.o.horiz ? ev.clientY : ev.clientX) - this._start;
        let target = Math.min(max, Math.max(0, this._orig_sz + diff));
        let frac = Math.min(0.9, Math.max(0.1, target / max));
        if (this.o.horiz) {
            cont.style.gridTemplateRows = `${frac}fr auto ${1-frac}fr`;
        } else {
            cont.style.gridTemplateColumns = `${frac}fr auto ${1-frac}fr`;
        }
    }
}
