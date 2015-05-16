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

// @require ymacs.js

DEFINE_CLASS("Ymacs_Frame", DlContainer, function(D, P, DOM) {

    var DBL_CLICK_SPEED = 300;

    var EX = DlException.stopEventBubbling;

    var LINE_DIV = DOM.createElement("div", null, { className: "line", innerHTML: "<br/>" });

    var BLINK_TIMEOUT = 225;

    D.DEFAULT_EVENTS = [ "onPointChange" ];

    D.DEFAULT_ARGS = {
        highlightCurrentLine : [ "highlightCurrentLine" , true ],
        buffer               : [ "buffer"               , null ],
        ymacs                : [ "ymacs"                , null ],
        isMinibuffer         : [ "isMinibuffer"         , false ],

        // override in DlWidget
        _focusable           : [ "focusable"            , true ],
        _fillParent          : [ "fillParent"           , true ]
    };

    D.CONSTRUCT = function() {
        this.__blinkCaret = this.__blinkCaret.$(this);
        this.__caretId = Dynarch.ID();
        this.redrawModelineWithTimer = this.redrawModeline.clearingTimeout(0, this);

        this.getElement().innerHTML = HTML;

        this.addEventListener({
            onDestroy    : this._on_destroy,
            onFocus      : this._on_focus,
            onBlur       : this._on_blur,
            onMouseDown  : this._on_mouseDown,
            onKeyDown    : this._on_keyDown,
            onKeyPress   : this._on_keyPress,
            onKeyUp      : this._on_keyUp,
            onResize     : this._on_resize.clearingTimeout(5),
            onMouseWheel : this._on_mouseWheel
        });

        this._dragSelectCaptures = {
            onMouseOver  : EX,
            onMouseOut   : EX,
            onMouseEnter : EX,
            onMouseLeave : EX,
            onMouseMove  : _dragSelect_onMouseMove.$(this),
            onMouseUp    : _dragSelect_onMouseUp.$(this)
        };

        this._bufferEvents = {
            onLineChange             : this._on_bufferLineChange.$(this),
            onInsertLine             : this._on_bufferInsertLine.$(this),
            onDeleteLine             : this._on_bufferDeleteLine.$(this),
            onPointChange            : this._on_bufferPointChange.$(this),
            onResetCode              : this._on_bufferResetCode.$(this),
            onOverwriteMode          : this._on_bufferOverwriteMode.$(this),
            onProgressChange         : this._on_bufferProgressChange.$(this),
            beforeInteractiveCommand : this._on_bufferBeforeInteractiveCommand.$(this),
            afterInteractiveCommand  : this._on_bufferAfterInteractiveCommand.$(this),
            onOverlayDelete          : this._on_bufferOverlayDelete.$(this)
        };

        this._moreBufferEvents = {
            onMessage               : this._on_bufferMessage.$(this),
            onOverlayChange         : this._on_bufferOverlayChange.$(this),
            afterInteractiveCommand : function(){
                if (this.__ensureCaretVisible)
                    this.ensureCaretVisible();
            }.$(this)
        };

        var buffer = this.buffer;
        this.buffer = null;
        if (buffer)
            this.setBuffer(buffer);
        if (!this.isMinibuffer && this.ymacs.cf_lineNumbers)
            this.toggleLineNumbers();

        this.getOverlaysContainer().onscroll = this._on_scroll.$(this);
    };

    var HTML = String.buffer(
        "<div class='Ymacs-frame-overlays'>",
        "<div class='Ymacs-frame-content'></div>",
        "</div>",
        "<div class='Ymacs_Modeline'></div>"
    ).get();

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

    P.vsplit = function(percent) {
        if (percent == null) percent = "50%";
        var cont   = this.parent,
        fr     = this.ymacs.createFrame({ buffer: this.buffer }),
        layout = new DlLayout(),
        rb     = new DlResizeBar({ widget: this, keepPercent: true, horiz: true, className: "Ymacs-splitbar-horiz" });
        if (this._resizeBar) {
            this._resizeBar._widget = layout;
            layout._resizeBar = this._resizeBar;
        }
        this._resizeBar = rb;
        cont.replaceWidget(this, layout);
        layout.packWidget(this, { pos: "top", fill: percent });
        layout.packWidget(rb, { pos: "top" });
        layout.packWidget(fr, { pos: "top", fill: "*" });
        cont.__doLayout();
        fr.centerOnCaret();
        return fr;
    };

    P.hsplit = function(percent) {
        if (percent == null) percent = "50%";
        var cont   = this.parent,
        fr     = this.ymacs.createFrame({ buffer: this.buffer }),
        layout = new DlLayout(),
        rb     = new DlResizeBar({ widget: this, keepPercent: true, className: "Ymacs-splitbar-vert" });
        if (this._resizeBar) {
            this._resizeBar._widget = layout;
            layout._resizeBar = this._resizeBar;
        }
        this._resizeBar = rb;
        cont.replaceWidget(this, layout);
        layout.packWidget(this, { pos: "left", fill: percent });
        layout.packWidget(rb, { pos: "left" });
        layout.packWidget(fr, { pos: "left", fill: "*" });
        cont.__doLayout();
        fr.centerOnCaret();
        return fr;
    };

    P.toggleLineNumbers = function() {
        this.condClass(this.__lineNumbers =! this.__lineNumbers, "Ymacs-line-numbers");
        if (this.buffer.transientMarker)
            this.buffer.ensureTransientMark();
    };

    function insertInText(div, col, el) {
        // this is for empty lines
        if (/^br$/i.test(div.firstChild.tagName)) {
            div.insertBefore(el, div.firstChild);
            return el;
        }
        var len = 0, OUT = {};
        function walk(div) {
            for (var i = div.firstChild; i; i = i.nextSibling) {
                if (i.nodeType == 3 /* TEXT */) {
                    var clen = i.length;
                    if (len + clen > col) {
                        var pos = col - len; // here we should insert it, relative to the current node
                        var next = i.splitText(pos);
                        div.insertBefore(el, next);
                        throw OUT;
                    }
                    else if (len + clen == col) {
                        // this case is simpler; it could have been treated
                        // above, but let's optimize a bit since there's no need
                        // to split the text.
                        div.insertBefore(el, i.nextSibling);
                        throw OUT;
                    }
                    len += clen;
                }
                else if (i.nodeType == 1 /* ELEMENT */) {
                    walk(i); // recurse
                }
            }
        };
        try {
            walk(div);
        }
        catch(ex) {
            if (ex === OUT)
                return el;
            throw ex;
        }
    };

    P.setMarkerAtPos = function(row, col) {
        if (!row.tagName) // accept an element as well
            row = this.getLineDivElement(row);
        if (row)
            return insertInText(row, col, DOM.createElement("span"));
    };

    P.__restartBlinking = function() {
        if (this.ymacs.cf_blinkCursor) {
            this.__stopBlinking();
            if (this.focusInside()) {
                this.__caretTimer = setTimeout(this.__blinkCaret, 2 * BLINK_TIMEOUT);
            }
        }
    };

    P.__stopBlinking = function() {
        if (this.ymacs.cf_blinkCursor) {
            clearTimeout(this.__caretTimer);
            this.__showCaret();
        }
    };

    P.__blinkCaret = function() {
        DOM.condClass(this.getCaretElement(), this.BLINKING = ! this.BLINKING, "Ymacs-caret");
        this.__caretTimer = setTimeout(this.__blinkCaret, BLINK_TIMEOUT);
    };

    P.__showCaret = function() {
        DOM.addClass(this.getCaretElement(), "Ymacs-caret");
    };

    P._unhoverLine = function() {
        if (this.__hoverLine != null) {
            DOM.delClass(this.getLineDivElement(this.__hoverLine), "Ymacs-current-line");
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
            DOM.addClass(this.getLineDivElement(rc.row), "Ymacs-current-line");
            this.__hoverLine = rc.row;
        }

        // hide stale carets :-\
        // mess everywhere.
        Array.$(this.getElement().querySelectorAll(".Ymacs-caret, #" + this.__caretId)).foreach(function(el){
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

        if (isActive)
            this.__restartBlinking();

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
        this.setContent(this.buffer.code.map(function(line, i){
            return this._getLineHTML(i).htmlEmbed("div", "line");
        }, this).join(""));
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

    P.coordinates = function(row, col) {
        var div = this.getLineDivElement(row);
        var span = this.setMarkerAtPos(div, col);
        var ret = { x: span.offsetLeft, y: div.offsetTop, h: div.offsetHeight };
        DOM.trash(span);
        return ret;
    };

    P.heightInLines = function() {
        return Math.floor(this.getOverlaysContainer().clientHeight / this.getContentElement().firstChild.offsetHeight);
    };

    P.setOuterSize = P.setSize = function(sz) {
        D.BASE.setOuterSize.apply(this, arguments);
        DOM.setOuterSize(this.getOverlaysContainer(), sz.x, sz.y - this.getModelineElement().offsetHeight);
        DOM.setOuterSize(this.getModelineElement(), sz.x);
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
        DOM.trash(this.getLineDivElement(row));
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

    P.getOverlayHTML = function(name, props) {
        if (props.line1 == props.line2 && props.col1 == props.col2) {
            this._on_bufferOverlayDelete(name, props);
            return null;
        }
        var p1 = this.coordinates(props.line1, props.col1);
        var p2 = this.coordinates(props.line2, props.col2);
        var p0 = this.__lineNumbers ? this.coordinates(props.line1, 0) : { x: 0, y: 0 };
        p1.x -= p0.x;
        p2.x -= p0.x;
        var str = String.buffer(
            "<div id='", this.getOverlayId(name), "' class='Ymacs_Overlay ", name,
            "' style='top:", p1.y, "px;left:", p0.x, "px'>"
        );
        if (props.line1 == props.line2) {
            str("<div class='", name, "' style='margin-left:", p1.x,
                "px; width:", p2.x - p1.x, "px;height:", p2.h, "px;'>&nbsp;</div>");
        } else {
            str("<div class='", name, "' style='margin-left:", p1.x, "px;height:", p1.h, "px;'>&nbsp;</div>");
            if (props.line2 - props.line1 > 1)
                str("<div class='", name, "' style='height:", p2.y - p1.y - p1.h, "px'></div>");
            str("<div class='", name, "' style='width:", p2.x, "px;height:", p2.h, "px;'>&nbsp;</div>");
        }
        str("</div>");
        return str.get();
    };

    P.getOverlaysCount = function() {
        return this.getOverlaysContainer().childNodes.length - 1; // XXX: subtract the div.content; we need to revisit this if we add new elements.
    };

    P._on_bufferOverlayChange = function(name, props, isNew) {
        var div = this.getOverlayHTML(name, props);
        if (div) {
            div = DOM.createFromHtml(div);
            var p = this.getOverlaysContainer(),
            old = !isNew && document.getElementById(this.getOverlayId(name));
            old ? p.replaceChild(div, old) : p.appendChild(div);
            // this.condClass(this.getOverlaysCount() > 0, "Ymacs_Frame-hasOverlays");
        }
    };

    P._on_bufferOverlayDelete = function(name, props, isNew) {
        DOM.trash(document.getElementById(this.getOverlayId(name)));
        // this.condClass(this.getOverlaysCount() > 0, "Ymacs_Frame-hasOverlays");
    };

    /* -----[ self events ]----- */

    P._on_destroy = function() {
        this.setBuffer(null);
        this.__stopBlinking();
    };

    P._on_focus = function() {
        window.focus();
        // console.log("FOCUS for %s", this.buffer.name);
        this.ymacs.setActiveFrame(this, true);
        this.addClass("Ymacs_Frame-active");
        if (!this.isMinibuffer) {
            this.buffer.cmd("goto_char", this.caretMarker.getPosition());
        }
        this.buffer.addEventListener(this._moreBufferEvents);
        this._redrawCaret();
        this.__restartBlinking();
    };

    P._on_blur = function() {
        // console.log("BLUR for %s", this.buffer.name);
        if (!this.isMinibuffer) {
            this.caretMarker.setPosition(this.buffer.caretMarker.getPosition());
        }
        this.buffer.removeEventListener(this._moreBufferEvents);
        this.__stopBlinking();
    };

    var CLICK_COUNT = 0, CLICK_COUNT_TIMER = null, CLICK_LAST_TIME = null;
    function CLEAR_CLICK_COUNT() { CLICK_COUNT = null };

    P._on_mouseDown = function(ev) {
        if (ev.ctrlKey && ev.shiftKey)
            return;
        clearTimeout(CLICK_COUNT_TIMER);
        CLICK_COUNT++;
        CLICK_COUNT_TIMER = CLEAR_CLICK_COUNT.delayed(DBL_CLICK_SPEED);

        this.__restartBlinking();
        var pos = ev.computePos(this.getContentElement()),
        rc = this.coordinatesToRowCol(pos.x, pos.y),
        buf = this.buffer;

        buf.clearTransientMark();
        buf.cmd("goto_char", buf._rowColToPosition(rc.row, rc.col));
        buf.callInteractively("keyboard_quit");
        if (CLICK_COUNT == 1) {
            buf.ensureTransientMark();
            DlEvent.captureGlobals(this._dragSelectCaptures);
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

        EX();
    };

    function _dragSelect_onMouseMove(ev) {
        var pos = ev.computePos(this.getContentElement()),
        rc = this.coordinatesToRowCol(pos.x, pos.y);
        this.buffer.cmd("goto_char", this.buffer._rowColToPosition(rc.row, rc.col));
        this.buffer.ensureTransientMark();
        this.ensureCaretVisible();
    };

    function _dragSelect_onMouseUp(ev) {
        DlEvent.releaseGlobals(this._dragSelectCaptures);
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
