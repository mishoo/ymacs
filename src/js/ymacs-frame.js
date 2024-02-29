//> This file is part of Ymacs, an Emacs-like editor for the Web
//> http://www.ymacs.org/
//>
//> Copyright (c) 2009-2012, Mihai Bazon, Dynarch.com.  All rights reserved.
//>
//> Redistribution and use in source and binary forms, with or without
//> modification, are permitted provided that the following conditions are
//> met:
//>
//>     * Redistributions of source code must retain the above copyright
//>       notice, this list of conditions and the following disclaimer.
//>
//>     * Redistributions in binary form must reproduce the above copyright
//>       notice, this list of conditions and the following disclaimer in
//>       the documentation and/or other materials provided with the
//>       distribution.
//>
//>     * Neither the name of Dynarch.com nor the names of its contributors
//>       may be used to endorse or promote products derived from this
//>       software without specific prior written permission.
//>
//> THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER “AS IS” AND ANY
//> EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
//> IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
//> PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER BE LIABLE
//> FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
//> CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
//> SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
//> INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
//> CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
//> ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
//> THE POSSIBILITY OF SUCH DAMAGE.

import "./ymacs.js";
import { DOM, Widget } from "./ymacs-utils.js";

DEFINE_CLASS("Ymacs_Frame", DlWidget, function(D, P, OLDOM) {

    var EX = DlException.stopEventBubbling;

    var LINE_DIV = DOM.fromHTML(`<div class="line"><br/></div>`);

    D.DEFAULT_EVENTS = [ "onPointChange" ];

    D.DEFAULT_ARGS = {
        highlightCurrentLine : [ "highlightCurrentLine" , true ],
        buffer               : [ "buffer"               , null ],
        ymacs                : [ "ymacs"                , null ],
        isMinibuffer         : [ "isMinibuffer"         , false ],

        // override in DlWidget
        _focusable           : [ "focusable"            , true ],
    };

    D.CONSTRUCT = function() {

        // <XXX> // during transition
        this.el = this.getElement();
        this.el._ymacs_object = this;
        // </XXX>

        this.__caretId = Dynarch.ID();
        this.redrawModelineWithTimer = this.redrawModeline.clearingTimeout(0, this);

        this.getElement().innerHTML = "<div class='Ymacs-frame-overlays'>"
            + "<div class='Ymacs-frame-content'></div>"
            + "</div>"
            + "<div class='Ymacs_Modeline'></div>";

        this.addEventListener({
            onDestroy    : this._on_destroy,
            onFocus      : this._on_focus,
            onBlur       : this._on_blur,
            onKeyDown    : this._on_keyDown,
            onKeyPress   : this._on_keyPress,
            onKeyUp      : this._on_keyUp,
            onResize     : this._on_resize.clearingTimeout(5),
            onMouseWheel : this._on_mouseWheel
        });

        this._dragSelectHandlers = {
            mousemove : _dragSelect_onMouseMove.bind(this),
            mouseup   : _dragSelect_onMouseUp.bind(this)
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
        DOM.on(this.getElement(), "mousedown", this._dragSelect_onMouseDown.bind(this));
    };

    P.focus = function(exitAllowed) {
        D.BASE.focus.call(this);
        if (exitAllowed instanceof Function) {
            this.removeEventListener("onBlur", this.__exitFocusHandler);
            this.addEventListener("onBlur", this.__exitFocusHandler = function(){
                if (exitAllowed.call(this.buffer)) {
                    this.removeEventListener("onBlur", this.__exitFocusHandler);
                } else {
                    this.focus.delayed(2, this, null);
                }
            });
        }
    };

    P.blur = function(force) {
        if (force)
            this.removeEventListener("onBlur", this.__exitFocusHandler);
        D.BASE.blur.call(this);
    };

    P.getOverlaysContainer = function() {
        return this.getElement().firstChild;
    };

    P.getModelineElement = function() {
        return this.getElement().childNodes[1];
    };

    P.getContentElement = function() {
        return this.getElement().firstChild.firstChild;
    };

    P.getCaretElement = function() {
        return document.getElementById(this.__caretId);
    };

    P.getLineDivElement = function(row) {
        return this.getContentElement().childNodes[row] || null;
    };

    P.ensureCaretVisible = function() {
        // return true if the scroll position was changed (that is, if
        // the caren't wasn't visible before the call).
        this._redrawCaret();
        var ret = false;

        var caret = this.getCaretElement();
        if (!caret)
            return ret;
        var div = this.getOverlaysContainer(), line = this.getLineDivElement(this.buffer._rowcol.row);

        // vertical
        var diff = line.offsetTop + line.offsetHeight - (div.scrollTop + div.clientHeight);
        if (diff > 0) {
            div.scrollTop += diff;
            ret = true;
        } else {
            diff = line.offsetTop - div.scrollTop;
            if (diff < 0) {
                div.scrollTop += diff;
                ret = true;
            }
        }

        // horizontal
        diff = caret.offsetLeft + caret.offsetWidth - (div.scrollLeft + div.clientWidth);
        // if (caret.offsetLeft + caret.offsetWidth < div.clientWidth)
        //         div.scrollLeft = 0;
        if (diff > 0) {
            div.scrollLeft += diff;
            ret = true;
        } else {
            diff = caret.offsetLeft - div.scrollLeft;
            if (diff < 0) {
                div.scrollLeft += diff;
                ret = true;
            }
        }
        return ret;
    };

    P.setBuffer = function(buffer) {
        if (this.buffer) {
            if (this.caretMarker && !this.isMinibuffer) {
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
            if (this.isMinibuffer) {
                this.caretMarker = buffer.caretMarker;
            } else {
                this.caretMarker = buffer.createMarker(buffer.caretMarker.getPosition(), false, "framecaret");
            }
            this._redrawBuffer();
            this._redrawCaret(true);
            this.centerOnCaret();
        }
    };

    P.centerOnCaret = function() {
        this.centerOnLine(this.buffer._rowcol.row);
    };

    P.centerOnLine = function(row) {
        var line = this.getLineDivElement(row), div = this.getOverlaysContainer();
        div.scrollTop = Math.round(line.offsetTop - div.clientHeight / 2 + line.offsetHeight / 2);
        // this._redrawBuffer();
    };

    P.setModelineContent = function(html) {
        this.getModelineElement().innerHTML = html;
    };

    P.deleteOtherFrames = function() {
        this.ymacs.keepOnlyFrame(this);
    };

    P.deleteFrame = function() {
        this.ymacs.deleteFrame(this);
    };

    P._split = function(horiz) {
        let ovdiv = this.getOverlaysContainer();
        let scroll = ovdiv.scrollTop;
        let fr = this.ymacs.createFrame({ buffer: this.buffer });
        let sc = new Ymacs_SplitCont({ horiz: horiz });
        sc.el.style.width = this.el.style.width;
        sc.el.style.height = this.el.style.height;
        this.el.style.removeProperty("width");
        this.el.style.removeProperty("height");
        this.getElement().replaceWith(sc.getElement());
        sc.add(this);
        sc.add(fr);
        ovdiv.scrollTop = scroll;
        fr.getOverlaysContainer().scrollTop = scroll;
        return fr;
    };

    P.vsplit = function() {
        return this._split(true);
    };

    P.hsplit = function(percent) {
        return this._split(false);
    };

    P.__showCaret = function() {
        OLDOM.addClass(this.getCaretElement(), "Ymacs-caret");
    };

    P._unhoverLine = function() {
        if (this.__hoverLine != null) {
            OLDOM.delClass(this.getLineDivElement(this.__hoverLine), "Ymacs-current-line");
            this.__hoverLine = null;
        }
    };

    P._redrawCaret = function(force) {
        if (this.isMinibuffer) force = true;
        var isActive = this.ymacs.getActiveFrame() === this;
        if (!force && !isActive)
            return;

        if (isActive && !this.isMinibuffer && this.focusInside())
            this.caretMarker.setPosition(this.buffer.caretMarker.getPosition());

        var rc = this.buffer._rowcol;

        if (this.highlightCurrentLine) {
            this._unhoverLine();
            OLDOM.addClass(this.getLineDivElement(rc.row), "Ymacs-current-line");
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
    };

    P._getLineHTML = function(row) {
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
    };

    P._redrawBuffer = function() {
        this.setContent(this.buffer.code.map((line, i) =>
            `<div class="line">${this._getLineHTML(i)}</div>`
        ).join(""));
    };

    P.coordinatesToRowCol = function(x, y) {
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
    };

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
    };

    P.coordinates = function(row, col) {
        var box = this.getContentElement().getBoundingClientRect();
        var div = this.getLineDivElement(row);
        return {
            x: textColX(div, col) - box.left,
            y: div.offsetTop,
            h: div.offsetHeight
        };
    };

    P.heightInLines = function() {
        return Math.floor(this.getOverlaysContainer().clientHeight / this.getContentElement().firstChild.offsetHeight);
    };

    P.setOuterSize = P.setSize = function(sz) {
        debugger;
        // var el = this.getElement();
        // if (sz.x != null) el.style.width = sz.x + "px";
        // if (sz.y != null) el.style.height = sz.y + "px";
    };

    P.redrawModeline = function(rc) {
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
    };

    /* -----[ event handlers ]----- */

    P._on_bufferLineChange = function(row) {
        var div = this.getLineDivElement(row);
        if (div) {
            //console.log("Redrawing line %d [%s]", row, this.buffer.code[row]);
            div.innerHTML = this._getLineHTML(row);
        }
    };

    P._on_bufferInsertLine = function(row, drawIt) {
        var div = LINE_DIV.cloneNode(true);
        this.getContentElement().insertBefore(div, this.getLineDivElement(row));
        if (drawIt) {
            div.innerHTML = this._getLineHTML(row);
        }
    };

    P._on_bufferDeleteLine = function(row) {
        OLDOM.trash(this.getLineDivElement(row));
    };

    P._on_bufferPointChange = function(rc, pos) {
        this._redrawCaret();
    };

    P._on_bufferResetCode = function() {
        this._redrawBuffer();
    };

    P._on_bufferOverwriteMode = function(om) {
        this.condClass(om, "Ymacs-overwrite-mode");
    };

    P._on_bufferMessage = function(type, text, html, timeout) {
        var anchor = this.isMinibuffer ? this.ymacs : this;
        var popup = Ymacs_Message_Popup.get(0);
        popup.popup({
            content : html ? text : text.htmlEscape(),
            widget  : anchor,
            anchor  : anchor.getElement(),
            align   : { prefer: "CC", fallX1: "CC", fallX2: "CC", fallY1: "CC", fallY2: "CC" }
        });
        popup.hide(timeout || 5000);
    };

    P._on_bufferBeforeInteractiveCommand = function() {
        this.__ensureCaretVisible = true;
        this._unhoverLine();
        Ymacs_Message_Popup.clearAll();
    };

    P._on_bufferAfterInteractiveCommand = function() {};

    P._on_bufferProgressChange = function() {
        this.redrawModelineWithTimer(null);
    };

    P.getOverlayId = function(name) {
        return this.id + "-ovl-" + name;
    };

    function normalizeRect(p) {
        return p.line1 > p.line2 || (p.line1 == p.line2 && p.col1 > p.col2) ?
            { line1: p.line2, col1: p.col2, line2: p.line1, col2: p.col1 } : p;
    }

    P.getOverlayHTML = function(name, props) {
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
        return str ? `<div id="${this.getOverlayId(name)}" class="Ymacs_Overlay ${name}">${str}</div>` : null;
    };

    P.getOverlaysCount = function() {
        return this.getOverlaysContainer().childNodes.length - 1; // XXX: subtract the div.content; we need to revisit this if we add new elements.
    };

    P._on_bufferOverlayChange = function(name, props) {
        let html = this.getOverlayHTML(name, Array.isArray(props) ? props : [ props ]);
        if (html) {
            let div = DOM.fromHTML(html);
            let old = document.getElementById(this.getOverlayId(name));
            old ? old.replaceWith(div) : this.getOverlaysContainer().appendChild(div);
            this.condClass(this.getOverlaysCount() > 0, "Ymacs_Frame-hasOverlays");
        } else {
            this._on_bufferOverlayDelete(name);
        }
    };

    P._on_bufferOverlayDelete = function(name) {
        OLDOM.trash(document.getElementById(this.getOverlayId(name)));
        this.condClass(this.getOverlaysCount() > 0, "Ymacs_Frame-hasOverlays");
    };

    /* -----[ self events ]----- */

    P._on_destroy = function() {
        this.setBuffer(null);
    };

    P._on_focus = function() {
        window.focus();
        this.ymacs.setActiveFrame(this, true);
        this.addClass("Ymacs_Frame-active");
        if (!this.isMinibuffer) {
            this.buffer.cmd("goto_char", this.caretMarker.getPosition());
        }
        this.buffer.addEventListener(this._moreBufferEvents);
        this._redrawCaret();
    };

    P._on_blur = function() {
        if (!this.isMinibuffer) {
            this.caretMarker.setPosition(this.buffer.caretMarker.getPosition());
        }
        this.buffer.removeEventListener(this._moreBufferEvents);
    };

    var DBL_CLICK_SPEED = 300;
    var CLICK_COUNT = 0, CLICK_COUNT_TIMER = null, CLICK_LAST_TIME = null;
    function CLEAR_CLICK_COUNT() { CLICK_COUNT = null };

    P._dragSelect_onMouseDown = function(ev) {
        this.focus();
        if (ev.ctrlKey && ev.shiftKey)
            return;
        ev.stopPropagation();
        clearTimeout(CLICK_COUNT_TIMER);
        CLICK_COUNT++;
        CLICK_COUNT_TIMER = CLEAR_CLICK_COUNT.delayed(DBL_CLICK_SPEED);

        let pos = DOM.mousePos(ev, this.getContentElement());
        let rc = this.coordinatesToRowCol(pos.x, pos.y);
        let buf = this.buffer;

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
    };

    function _dragSelect_onMouseMove(ev) {
        let pos = DOM.mousePos(ev, this.getContentElement());
        let rc = this.coordinatesToRowCol(pos.x, pos.y);
        this.buffer.cmd("goto_char", this.buffer._rowColToPosition(rc.row, rc.col));
        this.buffer.ensureTransientMark();
        this.ensureCaretVisible();
    };

    function _dragSelect_onMouseUp(ev) {
        DOM.off(window, this._dragSelectHandlers);
    };

    P._on_keyDown = function(ev) {
        if (this.ymacs.processKeyEvent(ev, false))
            EX();
    };

    P._on_keyPress = function(ev) {
        if (this.ymacs.processKeyEvent(ev, true))
            EX();
    };

    P._on_keyUp = function(ev) {
    };

    P._on_resize = function() {
        if (!this.destroyed) {
            this.centerOnCaret();
            this.redrawModelineWithTimer();
        }
    };

    P._on_scroll = function() {
        this.redrawModelineWithTimer();
    };

    P._on_mouseWheel = function(ev) {
        this.buffer._handleKeyEvent(ev);
        ev.domStop = true;
    };

    P.firstLineVisible = function() {
        var div = this.getOverlaysContainer();
        return this.coordinatesToRowCol(1, div.scrollTop + 1).row;
    };

    P.lastLineVisible = function() {
        var div = this.getOverlaysContainer();
        return this.coordinatesToRowCol(div.clientWidth - 2, div.scrollTop + div.clientHeight - 2).row;
    };

    P.scrollUp = function(lines) {
        var div = this.getOverlaysContainer();
        var line = Math.max(this.firstLineVisible() - lines, 0);
        line = this.getLineDivElement(line);
        div.scrollTop = line.offsetTop;
        this.__ensureCaretVisible = false;
    };

    P.scrollDown = function(lines) {
        var div = this.getOverlaysContainer();
        var line = Math.min(this.firstLineVisible() + lines, this.buffer.code.length - 1);
        line = this.getLineDivElement(line);
        div.scrollTop = line.offsetTop;
        this.__ensureCaretVisible = false;
    };

});

DEFINE_CLASS("Ymacs_Message_Popup", DlPopup, function(D, P) {
    D.FIXARGS = function(args) {
        args.focusable = false;
        args.autolink = false;
        args.zIndex = 5000;
    };
});

class Ymacs_SplitCont extends Widget {
    static options = {
        horiz: false
    };
    initClassName() {
        return "Ymacs_SplitCont " + (this.o.horiz ? "horiz" : "vert");
    }
    createElement() {
        super.createElement();
        this._dragHandlers = {
            mousemove : this._onMouseMove.bind(this),
            mouseup   : this._onMouseUp.bind(this),
        };
        DOM.on(this.getElement(), "mousedown", this._onMouseDown.bind(this));
    }
    _onMouseDown(ev) {
        let first = this.getContentElement().children[0];
        this._start = this.o.horiz ? ev.clientY : ev.clientX;
        this._orig_sz = this.o.horiz ? first.offsetHeight : first.offsetWidth;
        this._orig_frac = this._1stChildFr();
        this.addClass("dragging");
        ev.stopPropagation();
        DOM.on(window, this._dragHandlers);
        DOM.overlayOn(this.o.horiz ? "Ymacs_Resize_horiz" : "Ymacs_Resize_vert");
    }
    _onMouseUp(ev) {
        DOM.off(window, this._dragHandlers);
        this.delClass("dragging");
        DOM.overlayOff();
    }
    _onMouseMove(ev) {
        let cont = this.getElement();
        let max = this.o.horiz ? cont.offsetHeight : cont.offsetWidth;
        let diff = (this.o.horiz ? ev.clientY : ev.clientX) - this._start;
        let a = this.getContentElement().children[0];
        let b = this.getContentElement().children[1];
        let target = Math.min(max, Math.max(0, this._orig_sz + diff));
        let frac = Math.min(0.9, Math.max(0.1, target / max));
        if (this.o.horiz) {
            a.style.height = (frac * 100) + "%";
            b.style.height = ((1 - frac) * 100) + "%";
        } else {
            a.style.width = (frac * 100) + "%";
            b.style.width = ((1 - frac) * 100) + "%";
        }
    }
    _1stChildFr() {
        let a = this.getContentElement().children[0];
        let b = this.getContentElement().children[1];
        if (this.o.horiz) {
            return (a.offsetHeight + b.offsetHeight) / a.offsetHeight;
        } else {
            return (a.offsetWidth + b.offsetWidth) / a.offsetWidth;
        }
    }
}

window.Ymacs_SplitCont = Ymacs_SplitCont; // XXX.
