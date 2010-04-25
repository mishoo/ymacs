//> This file is part of Ymacs, an Emacs-like editor for the Web
//> http://www.ymacs.org/
//>
//> Copyright (c) 2009-2010, Mihai Bazon, Dynarch.com.  All rights reserved.
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

        var MAX_LINES_IN_DOM = 100;
        var ADD_LINES_IN_DOM = 40;
        var MIN_LINES_IN_DOM = 5;

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
                //this.redrawModelineWithTimer = this.redrawModeline;

                this.getElement().innerHTML = HTML;

                this.addEventListener({
                        onDestroy   : this._on_destroy,
                        onFocus     : this._on_focus,
                        onBlur      : this._on_blur,
                        onMouseDown : this._on_mouseDown,
                        onKeyDown   : this._on_keyDown,
                        onKeyPress  : this._on_keyPress,
                        onKeyUp     : this._on_keyUp,
                        onResize    : this._on_resize
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
                        //onPointChange            : this._on_bufferPointChange.$(this),
                        onResetCode              : this._on_bufferResetCode.$(this),
                        onOverwriteMode          : this._on_bufferOverwriteMode.$(this),
                        onProgressChange         : this._on_bufferProgressChange.$(this),
                        beforeInteractiveCommand : this._on_bufferBeforeInteractiveCommand.$(this),
                        onOverlayDelete          : this._on_bufferOverlayDelete.$(this),
                        afterRedraw              : this._on_bufferAfterRedraw.$(this)
                };

                this._moreBufferEvents = {
                        onOverlayChange          : this._on_bufferOverlayChange.clearingTimeout(10, this),
                        afterInteractiveCommand  : this._on_bufferAfterInteractiveCommand.$(this),
                        onMessage                : this._on_bufferMessage.$(this)
                };

                var buffer = this.buffer;
                this.buffer = null;
                if (buffer)
                        this.setBuffer(buffer);
                if (!this.isMinibuffer && this.ymacs.cf_lineNumbers)
                        this.toggleLineNumbers();
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
                var a = this.getContentElement().childNodes;
                row -= this._firstLineInDOM;
                if (row >= 0 && row < a.length)
                        return a[row];
        };

        P.firstLineVisible = function() {
                var ov_cont = this.getOverlaysContainer();
                var top = ov_cont.scrollTop;
                var line = this._firstLineInDOM;
                for (var i = this.getContentElement().firstChild; i; i = i.nextSibling) {
                        if (i.offsetTop >= top)
                                return line;
                        line++;
                }
        };

        P.lastLineVisible = function() {
                var ov_cont = this.getOverlaysContainer();
                var bottom = ov_cont.scrollTop + ov_cont.clientHeight;
                var line = this._lastLineInDOM;
                for (var i = this.getContentElement().lastChild; i; i = i.previousSibling) {
                        if (i.offsetTop + i.offsetHeight <= bottom)
                                return line;
                        line--;
                }
        };

        P.isLineVisible = function(line) {
                return line >= this.firstLineVisible() && line < this.lastLineVisible();
        };

        P.ensureCaretVisible = function(reset) {
                if (reset)
                        this.caretMarker.setPosition(this.buffer.caretMarker.getPosition());
                var line = this.caretMarker.getRowCol().row;
                this._ensure_line_in_dom(line);
                this._redrawCaret();

                var caret = this.getCaretElement();
                if (!caret)
                        return;
                var div = this.getOverlaysContainer();
                line = this.getLineDivElement(line);

                // vertical
                var diff = line.offsetTop + line.offsetHeight - (div.scrollTop + div.clientHeight);
                if (diff > 0) {
                        div.scrollTop += diff;
                } else {
                        diff = line.offsetTop - div.scrollTop;
                        if (diff < 0) {
                                div.scrollTop += diff;
                        }
                }

                // horizontal
                diff = caret.offsetLeft + caret.offsetWidth - (div.scrollLeft + div.clientWidth);
                // if (caret.offsetLeft + caret.offsetWidth < div.clientWidth)
                //         div.scrollLeft = 0;
                if (diff > 0) {
                        div.scrollLeft += diff;
                } else {
                        diff = caret.offsetLeft - div.scrollLeft;
                        if (diff < 0)
                                div.scrollLeft += diff;
                }

                this._refill_dom();
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
                        this.buffer.addEventListener(this._bufferEvents);
                        if (this.focusInside()) {
                                buffer.addEventListener(this._moreBufferEvents);
                        }
                        if (this.isMinibuffer) {
                                this.caretMarker = buffer.caretMarker;
                        } else {
                                this.caretMarker = buffer.createMarker(buffer.caretMarker.getPosition());
                        }
                        this._redrawBuffer();
                        this._redrawCaret(true);
                        this.centerOnCaret();
                }
        };

        P.centerOnCaret = function() {
                this.centerOnLine(this.caretMarker.getRowCol().row);
        };

        P.centerOnCaretIfNotVisible = function() {
                // <XXX>: sucky.  We should refactor somehow such that
                // the active frame's caret is ALWAYS in sync with the buffer.
                this.caretMarker.setPosition(this.buffer.caretMarker.getPosition());
                // </XXX>
                var line = this.caretMarker.getRowCol().row;
                if (!this.isLineVisible(line))
                        this.centerOnLine(line);
        };

        P.centerOnLine = function(row, pos) {
                if (pos == null)
                        pos = "center";
                var ov_cont = this.getOverlaysContainer(), self = this;
                self._ensure_line_in_dom(row);
                var div = self.getLineDivElement(row);
                switch (pos) {
                    case "center":
                        ov_cont.scrollTop = Math.round(div.offsetTop - ov_cont.clientHeight / 2 + div.offsetHeight / 2);
                        break;
                    case "top":
                        ov_cont.scrollTop = div.offsetTop;
                        break;
                    case "bottom":
                        ov_cont.scrollTop = div.offsetTop + div.offsetHeight - ov_cont.clientHeight;
                        break;
                }
                self._refill_dom();
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

        P.vsplit = function() {
                var cont   = this.parent,
                    fr     = this.ymacs.createFrame({ buffer: this.buffer }),
                    layout = new DlLayout(),
                    rb     = new DlResizeBar({ widget: this, keepPercent: true, horiz: true, className: "Ymacs-splitbar-horiz" });
                if (this._resizeBar)
                        this._resizeBar._widget = layout;
                this._resizeBar = rb;
                cont.replaceWidget(this, layout);
                layout.packWidget(this, { pos: "top", fill: "50%" });
                layout.packWidget(rb, { pos: "top" });
                layout.packWidget(fr, { pos: "top", fill: "*" });
                cont.__doLayout();
                fr.centerOnCaret();
        };

        P.hsplit = function() {
                var cont   = this.parent,
                    fr     = this.ymacs.createFrame({ buffer: this.buffer }),
                    layout = new DlLayout(),
                    rb     = new DlResizeBar({ widget: this, keepPercent: true, className: "Ymacs-splitbar-vert" });
                if (this._resizeBar)
                        this._resizeBar._widget = layout;
                this._resizeBar = rb;
                cont.replaceWidget(this, layout);
                layout.packWidget(this, { pos: "left", fill: "50%" });
                layout.packWidget(rb, { pos: "left" });
                layout.packWidget(fr, { pos: "left", fill: "*" });
                cont.__doLayout();
                fr.centerOnCaret();
        };

        P.toggleLineNumbers = function() {
                this.condClass(this.__lineNumbers =! this.__lineNumbers, "Ymacs-line-numbers");
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
                this.__stopBlinking();
                if (this.focusInside()) {
                        this.__caretTimer = setTimeout(this.__blinkCaret, 2 * BLINK_TIMEOUT);
                }
        };

        P.__stopBlinking = function() {
                clearTimeout(this.__caretTimer);
                this.__showCaret();
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
                var isActive = this.ymacs.getActiveFrame() === this;
                if (!force && !isActive)
                        return;

                var rc = this.caretMarker.getRowCol();

                if (this.highlightCurrentLine) {
                        this._unhoverLine();
                        DOM.addClass(this.getLineDivElement(rc.row), "Ymacs-current-line");
                        this.__hoverLine = rc.row;
                }

                // redraw the line where the caret was previously, so that it disappears from there
                if (this.__prevCaretLine != null) {
                        this._on_bufferLineChange(this.__prevCaretLine);
                }

                // redraw current line if it's different
                if (this.__prevCaretLine != rc.row) {
                        this.__prevCaretLine = rc.row;
                        this._on_bufferLineChange(rc.row);
                }

                // var caret = this.getCaretElement();
                // if (caret)
                //         DOM.strip(caret);
                // this._on_bufferLineChange(rc.row);

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

        P._getCodeSlice = function(min, max) {
                var len = this.buffer.code.length;
                min = Math.min(Math.max(min, 0), len);
                max = Math.min(max, len);
                return { first: min, last: max, code: max > min ? this.buffer.code.slice(min, max) : null };
        };

        P._getHTMLSlice = function(min, max) {
                var slice = this._getCodeSlice(min, max), html = null;
                if (slice.code) {
                        var buf = String.buffer();
                        (slice.last - slice.first).times(function(i){
                                buf("<div class='line'>", this._getLineHTML(slice.first + i), "</div>");
                        }, this);
                        html = buf.get();
                }
                return {
                        first : slice.first,
                        last  : slice.last,
                        html  : html
                };
        };

        // XXX: this is probably not optimal
        P._getDFSlice = function(min, max) {
                var slice = this._getHTMLSlice(min, max);
                var df = null;
                if (slice.html) {
                        var div = DOM.createFromHtml("<div>" + slice.html + "</div>");
                        df = document.createDocumentFragment();
                        while (div.firstChild)
                                df.appendChild(div.firstChild);
                        DOM.trash(div);
                }
                slice.df = df;
                return slice;
        };

        P._resetFirstLineInDOM = function(line) {
                this._firstLineInDOM = line;
                var div = this.getContentElement().firstChild;
                if (div)
                        div.style.counterReset = "ymacs-line " + line;
        };

        P._resetLastLineInDOM = function(line) {
                this._lastLineInDOM = line;
        };

        P._redrawBuffer = function() {
                var slice = this._getHTMLSlice(0, MAX_LINES_IN_DOM);
                this._resetFirstLineInDOM(slice.first);
                this._resetLastLineInDOM(slice.last);
                this.setContent(slice.html);
        };

        P.coordinatesToRowCol = function(x, y) {
                function findLine(r1, r2) {
                        if (r1 == r2)
                                return r1;
                        var row = Math.floor((r1 + r2) / 2),
                            div = self.getLineDivElement(row),
                            y1  = div.offsetTop,
                            y2  = y1 + div.offsetHeight - 1;
                        if (y2 < y)
                                return findLine(row + 1, r2);
                        if (y < y1)
                                return findLine(r1, row - 1);
                        return row;
                };
                function findCol(c1, c2) {
                        if (c1 == c2)
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
                    row = findLine(this._firstLineInDOM, this._lastLineInDOM - 1),
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
                this.setModelineContent(this.buffer.renderModelineContent(rc || this.caretMarker.getRowCol()));
        };

        /* -----[ event handlers ]----- */

        P._on_bufferLineChange = function(row) {
                var div = this.getLineDivElement(row);
                if (div) {
                        div.innerHTML = this._getLineHTML(row);
                }
        };

        P._on_bufferInsertLine = function(row, drawIt) {
                var inserted = row >= this._firstLineInDOM && row <= this._lastLineInDOM;
                if (inserted) {
                        var div = LINE_DIV.cloneNode(true);
                        this.getContentElement().insertBefore(div, this.getLineDivElement(row));
                        if (drawIt) {
                                div.innerHTML = this._getLineHTML(row);
                        }
                }
                if (row < this._firstLineInDOM)
                        this._resetFirstLineInDOM(this._firstLineInDOM + 1);
                if (row <= this._lastLineInDOM) {
                        //this._resetLastLineInDOM(this._lastLineInDOM + 1);
                        DOM.trash(this.getContentElement().lastChild);
                }
                //this._refill_dom();
        };

        P._on_bufferDeleteLine = function(row) {
                var div = this.getLineDivElement(row);
                if (div && this.getContentElement().childNodes.length > 1) {
                        DOM.trash(div);
                        if (row < this._firstLineInDOM)
                                this._resetFirstLineInDOM(this._firstLineInDOM - 1);
                        if (row < this._lastLineInDOM)
                                this._resetLastLineInDOM(this._lastLineInDOM - 1);
                        // if (this._firstLineInDOM == this._lastLineInDOM) {
                        //         // no more lines
                        //         this._firstLineInDOM--;
                        // }
                }
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
                this._unhoverLine();
                Ymacs_Message_Popup.clearAll();
        };

        P._on_bufferAfterInteractiveCommand = function() {
                this.ensureCaretVisible(true);
        };

        P._on_bufferAfterRedraw = function() {
                this._refill_dom();
        };

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
                // <hacks>
                if (props.line2 < this._firstLineInDOM || props.line1 >= this._lastLineInDOM) {
                        // totally out of sight
                        this._on_bufferOverlayDelete(name, props);
                        return null;
                }
                props = Object.makeCopy(props);
                if (props.line1 < this._firstLineInDOM) {
                        props.line1 = this._firstLineInDOM;
                        props.col1 = 0;
                }
                if (props.line2 >= this._lastLineInDOM) {
                        props.line2 = this._lastLineInDOM - 1;
                        props.col2 = 0;
                }
                // </hacks>
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
                            old = !isNew && $(this.getOverlayId(name));
                        old ? p.replaceChild(div, old) : p.appendChild(div);
                        // this.condClass(this.getOverlaysCount() > 0, "Ymacs_Frame-hasOverlays");
                }
        };

        P._on_bufferOverlayDelete = function(name, props, isNew) {
                DOM.trash($(this.getOverlayId(name)));
                // this.condClass(this.getOverlaysCount() > 0, "Ymacs_Frame-hasOverlays");
        };

        /* -----[ self events ]----- */

        P._on_destroy = function() {
                this.setBuffer(null);
                this.__stopBlinking();
        };

        P._on_focus = function() {
                window.focus();
                this.ymacs.setActiveFrame(this, true);
                this.addClass("Ymacs_Frame-active");
                if (!this.isMinibuffer) {
                        this.buffer.cmd("goto_char", this.caretMarker.getPosition());
                }
                this.buffer.addEventListener(this._moreBufferEvents);
                this.__restartBlinking();
        };

        P._on_blur = function() {
                if (!this.isMinibuffer) {
                        this.caretMarker.setPosition(this.buffer.caretMarker.getPosition());
                }
                this.buffer.removeEventListener(this._moreBufferEvents);
                this.__stopBlinking();
        };

        var CLICK_COUNT = 0, CLICK_COUNT_TIMER = null, CLICK_LAST_TIME = null;
        function CLEAR_CLICK_COUNT() { CLICK_COUNT = null };

        P._on_mouseDown = function(ev) {
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
                this.ensureCaretVisible(true);
        };

        function _dragSelect_onMouseUp(ev) {
                DlEvent.releaseGlobals(this._dragSelectCaptures);
        };

        P._on_keyDown = function(ev) {
                if (!is_gecko) {
                        var ki = window.KEYBOARD_INSANITY, code = ev.keyCode;
                        if (code in ki.modifiers)
                                EX();
                        if ((code in ki.letters || code in ki.digits || code in ki.symbols) && !(ev.ctrlKey || ev.altKey)) {
                                return; // to be handled by the upcoming keypress event
                        }
                        ev.charCode = ki.getCharCode(code, ev.shiftKey);
                        if (ev.charCode)
                                ev.keyCode = 0;
                        if (this.buffer._handleKeyEvent(ev))
                                EX();
                }
        };

        P._on_keyPress = function(ev) {
                if (!is_gecko)
                        ev.keyCode = 0;
                if (this.buffer._handleKeyEvent(ev))
                        EX();
        };

        P._on_keyUp = function(ev) {
        };

        P._on_resize = function() {
                this.centerOnCaret.delayed(1, this);
        };

        P._ensure_line_in_dom = function(line) {
                var first_dom = this._firstLineInDOM,
                    last_dom = this._lastLineInDOM,
                    cont = this.getContentElement(), slice;
                // // case 1.
                // if (this.getLineDivElement(line))
                //         return;
                // case 2.
                if (Math.abs(line - first_dom) < MIN_LINES_IN_DOM) {
                        slice = this._getDFSlice(first_dom - ADD_LINES_IN_DOM, first_dom);
                        if (slice.df) {
                                // must add lines at TOP; this WILL scroll.
                                var ovc = this.getOverlaysContainer(),
                                    first = this.getLineDivElement(this.firstLineVisible()),
                                    scrollDiff = first.offsetTop - ovc.scrollTop;
                                cont.insertBefore(slice.df, cont.firstChild);
                                this._resetFirstLineInDOM(slice.first);
                                ovc.scrollTop = first.offsetTop - scrollDiff;
                        }
                }
                // case 3.
                else if (Math.abs(line - last_dom) < MIN_LINES_IN_DOM) {
                        slice = this._getDFSlice(last_dom, last_dom + ADD_LINES_IN_DOM);
                        if (slice.df) {
                                // must add lines at BOTTOM
                                cont.appendChild(slice.df);
                                this._resetLastLineInDOM(slice.last);
                        }
                }
                // case 4.
                else if (line < first_dom || line >= last_dom) {
                        // redraw the whole block where that line falls into the center
                        slice = this._getHTMLSlice(line - ADD_LINES_IN_DOM,
                                                   line + ADD_LINES_IN_DOM);
                        this._resetFirstLineInDOM(slice.first);
                        this._resetLastLineInDOM(slice.last);
                        this.setContent(slice.html);
                }
        };

        P._refill_dom = function() {
                this._refill_dom_top();
                this._refill_dom_bottom();
        };

        P._refill_dom_top = function() {
                var first_visible = this.firstLineVisible(),
                    first_dom = this._firstLineInDOM,
                    diff = first_visible - first_dom,
                    cont = this.getContentElement(),
                    ovc = this.getOverlaysContainer(),
                    first = this.getLineDivElement(first_visible),
                    scrollDiff = first.offsetTop - ovc.scrollTop;
                if (diff < ADD_LINES_IN_DOM && diff >= 0) {
                        // should add some
                        var slice = this._getDFSlice(first_dom - ADD_LINES_IN_DOM, first_dom);
                        if (slice.df) {
                                // this WILL scroll
                                cont.insertBefore(slice.df, cont.firstChild);
                                this._resetFirstLineInDOM(slice.first);
                        }
                }
                else if (diff > MAX_LINES_IN_DOM) {
                        // should remove some
                        var div = this.getLineDivElement(first_visible - ADD_LINES_IN_DOM);
                        while (div) {
                                var prev = div.previousSibling;
                                DOM.trash(div);
                                div = prev;
                                this._resetFirstLineInDOM(this._firstLineInDOM + 1);
                        }
                }
                ovc.scrollTop = first.offsetTop - scrollDiff;
        };

        P._refill_dom_bottom = function() {
                var last_visible = this.lastLineVisible(),
                    last_dom = this._lastLineInDOM,
                    diff = last_dom - last_visible,
                    cont = this.getContentElement();
                if (diff < ADD_LINES_IN_DOM && diff >= 0) {
                        // should add some more
                        var slice = this._getDFSlice(last_dom, last_dom + ADD_LINES_IN_DOM);
                        if (slice.df) {
                                cont.appendChild(slice.df);
                                this._resetLastLineInDOM(slice.last);
                        }
                }
                else if (diff > MAX_LINES_IN_DOM) {
                        // should remove some
                        var div = this.getLineDivElement(last_visible + ADD_LINES_IN_DOM);
                        while (div) {
                                var next = div.nextSibling;
                                DOM.trash(div);
                                div = next;
                                this._resetLastLineInDOM(this._lastLineInDOM - 1);
                        }
                }
        };

        var TMPL_STATS = String.template(
                "DOM: $first_dom -> $last_dom, VISIBLE: $first -> $last, WEIGHT: $weight, LEN: $len"
        );

        P._fill_stats = function() {
                return TMPL_STATS({
                        first_dom : this._firstLineInDOM,
                        first     : this.firstLineVisible(),
                        last_dom  : this._lastLineInDOM,
                        last      : this.lastLineVisible(),
                        weight    : this.getContentElement().childNodes.length,
                        len       : this.buffer.code.length
                });
        };

});

DEFINE_CLASS("Ymacs_Message_Popup", DlPopup, function(D, P) {
        D.FIXARGS = function(args) {
                args.focusable = false;
                args.autolink = false;
                args.zIndex = 5000;
        };
});
