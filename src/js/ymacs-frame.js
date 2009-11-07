// @require ymacs.js

DEFINE_CLASS("Ymacs_Frame", DlContainer, function(D, P, DOM) {

        D.DEFAULT_EVENTS = [ "onPointChange" ];

        D.DEFAULT_ARGS = {
                highlightCurrentLine : [ "highlightCurrentLine" , true ],
                buffer               : [ "buffer"               , null ],
                ymacs                : [ "ymacs"                , null ],

                // override in DlContainer
                _scrollBars          : [ "scroll"               , true ],

                // override in DlWidget
                _focusable           : [ "focusable"            , true ],
                _fillParent          : [ "fillParent"           , true ]
        };

        var BLINK_TIMEOUT = 200;

        D.CONSTRUCT = function() {
                this.__blinkCaret = this.__blinkCaret.$(this);
                var tmp = this._bufferEvents = {
                        onLineChange             : this._on_bufferLineChange.$(this),
                        onInsertLine             : this._on_bufferInsertLine.$(this),
                        onDeleteLine             : this._on_bufferDeleteLine.$(this),
                        onPointChange            : this._on_bufferPointChange.$(this),
                        onResetCode              : this._on_bufferResetCode.$(this),
                        onOverwriteMode          : this._on_bufferOverwriteMode.$(this),
                        onOverlayChange          : this._on_bufferOverlayChange.$(this),
                        onOverlayDelete          : this._on_bufferOverlayDelete.$(this),
                        beforeInteractiveCommand : this._on_bufferBeforeInteractiveCommand.$(this)
                };
                tmp = this._moreBufferEvents = Object.makeCopy(tmp);
                tmp.onMessage = this._on_bufferMessage.$(this);
                var buffer = this.buffer;
                this.buffer = null;
                if (buffer)
                        this.setBuffer(buffer);
                this.addEventListener("onResize", this.centerOnCaret);
        };

        P.initDOM = function() {
                D.BASE.initDOM.apply(this, arguments);
                this.getElement().innerHTML = "<div class='content'></div><div class='Ymacs-caret'>&nbsp;</div>";
                this.addEventListener({
                        onDestroy   : this._on_destroy.$(this),
                        onFocus     : this._on_focus.$(this),
                        onBlur      : this._on_blur.$(this),
                        onMouseDown : this._on_mouseDown.$(this)
                });
        };

        P.getContentElement = function() {
                return this.getElement().firstChild;
        };

        P.getCaretElement = function() {
                return this.getElement().childNodes[1];
        };

        P.getLineDivElement = function(row) {
                return this.getContentElement().childNodes[row];
        };

        P.ensureCaretVisible = function() {
                this._redrawCaret();
                var caret = this.getCaretElement(), div = this.getElement();
                // vertical
                var diff = caret.offsetTop + caret.offsetHeight - (div.scrollTop + div.clientHeight);
                if (diff > 0) {
                        div.scrollTop += diff;
                } else {
                        diff = caret.offsetTop - div.scrollTop;
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
        };

        P.setBuffer = function(buffer) {
                if (this.buffer) {
                        if (this.caretMarker) {
                                this.caretMarker.destroy();
                                this.caretMarker = null;
                        }
                        this.buffer.removeEventListener(this._moreBufferEvents);
                }
                this.buffer = buffer;
                if (buffer) {
                        buffer.addEventListener(this.focusInside() ? this._moreBufferEvents : this._bufferEvents);
                        this.caretMarker = buffer.createMarker(buffer.caretMarker.getPosition());
                        this._redrawBuffer();
                        this._redrawCaret(true);
                        this.ensureCaretVisible();
                }
        };

        P.centerOnCaret = function() {
                this.centerOnLine(this.buffer._rowcol.row);
        };

        P.centerOnLine = function(row) {
                var line = this.getLineDivElement(row), div = this.getElement();
                div.scrollTop = Math.round(line.offsetTop - div.clientHeight / 2 + line.offsetHeight / 2);
                // this._redrawBuffer();
        };

        P.deleteOtherFrames = function() {
                this.ymacs.keepOnlyFrame(this);
        };

        P.vsplit = function() {
                var cont = this.parent;

                // we need a new container for this frame (c1) and one for the sister frame (c2)
                var c1 = new DlContainer({});
                var c2 = new DlContainer({});
                c1.appendWidget(this);

                // clone the frame
                var fr = this.ymacs.createFrame({ parent: c2, buffer: this.buffer });

                // now create a layout, pack c1 and c2 and a resize bar
                var layout = new DlLayout({ parent: cont });
                layout.packWidget(c1, { pos: "top", fill: "50%" });
                var rb = new DlResizeBar({ widget: c1, horiz: true });
                layout.packWidget(rb, { pos: "top" });
                layout.packWidget(c2, { pos: "top", fill: "*" });

                // update dimensions
                cont.__doLayout();
        };

        P.hsplit = function() {
                var cont = this.parent;

                // we need a new container for this frame (c1) and one for the sister frame (c2)
                var c1 = new DlContainer({});
                var c2 = new DlContainer({});
                c1.appendWidget(this);

                // clone the frame
                var fr = this.ymacs.createFrame({ parent: c2, buffer: this.buffer });

                // now create a layout, pack c1 and c2 and a resize bar
                var layout = new DlLayout({ parent: cont });
                layout.packWidget(c1, { pos: "left", fill: "50%" });
                var rb = new DlResizeBar({ widget: c1 });
                layout.packWidget(rb, { pos: "left" });
                layout.packWidget(c2, { pos: "left", fill: "*" });

                // update dimensions
                cont.__doLayout();
        };

        P.__restartBlinking = function() {
                DOM.delClass(this.getCaretElement(), "Ymacs-caret-blink");
                this.__stopBlinking();
                this.__caretTimer = setInterval(this.__blinkCaret, BLINK_TIMEOUT);
        };

        P.__stopBlinking = function() {
                clearInterval(this.__caretTimer);
        };

        P.__blinkCaret = function() {
                DOM.condClass(this.getCaretElement(), this.BLINKING =! this.BLINKING, "Ymacs-caret-blink");
        };

        P._redrawCaret = function(force) {
                if (!force && this.ymacs.getActiveFrame() !== this)
                        return;
                var rc = this.buffer._rowcol, caret = this.getCaretElement(), w = caret.offsetWidth, h = caret.offsetHeight;
                caret.style.left = (w * rc.col) + "px";
                caret.style.top = (h * rc.row) + "px";
                this.__restartBlinking();
                var ch = this.buffer.charAtRowCol(rc.row, rc.col);
                if (ch == "\n" || !ch)
                        ch = "&nbsp;";
                this.getCaretElement().innerHTML = ch;
                if (this.highlightCurrentLine) {
                        if (this.__hoverLine != null)
                                DOM.delClass(this.getLineDivElement(this.__hoverLine), "Ymacs-current-line");
                        DOM.addClass(this.getLineDivElement(rc.row), "Ymacs-current-line");
                        this.__hoverLine = rc.row;
                }
                this.callHooks("onPointChange", rc.row, rc.col);
        };

        P._getLineHTML = function(row) {
                return this.buffer.formatLineHTML(row);
        };

        P._redrawBuffer = function() {
                this.setContent(this.buffer.code.map(function(line, i){
                        return this._getLineHTML(i).htmlEmbed("div", "line");
                }, this).join(""));
        };

        P.coordinates = function(row, col) {
                var caret = this.getCaretElement(), w = caret.offsetWidth, h = caret.offsetHeight;
                return { x: col * w, y: row * h, cw: w, ch: h };
        };

        P.heightInLines = function() {
                return Math.floor(this.getElement().clientHeight / this.getCaretElement().offsetHeight);
        };

        /* -----[ event handlers ]----- */

        P._on_bufferLineChange = function(row) {
                var div = this.getLineDivElement(row);
                if (div) {
                        div.innerHTML = this._getLineHTML(row);
                }
        };

        P._on_bufferInsertLine = function(row, drawIt) {
                var div;
                if (drawIt)
                        div = DOM.createFromHtml(this._getLineHTML(row).htmlEmbed("div", "line"));
                else
                        div = DOM.createElement("div", null, { className: "line" });
                this.getContentElement().insertBefore(div, this.getLineDivElement(row));
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

        P._on_bufferMessage = function(type, text, html) {
                var popup = Ymacs_Message_Popup.get(0);
                popup.popup({
                        content : html ? text : text.htmlEscape(),
                        widget  : this,
                        anchor  : this.getElement(),
                        align   : { prefer: "CC", fallX1: "CC", fallX2: "CC", fallY1: "CC", fallY2: "CC" }
                });
                popup.hide(5000);
        };

        P._on_bufferBeforeInteractiveCommand = function() {
                Ymacs_Message_Popup.clearAll();
        };

        P.getOverlayId = function(name) {
                return this.id + "-ovl-" + name;
        };

        P.getOverlayHTML = function(name, props) {
                var p1 = this.coordinates(props.line1, props.col1);
                var p2 = this.coordinates(props.line2 - 1, props.col2);
                var str = String.buffer(
                        "<div id='", this.getOverlayId(name), "' class='Ymacs_Overlay ", name,
                        "' style='top:", p1.y, "px'>"
                );
                if (props.line1 == props.line2) {
                        str("<div class='", name, "' style='margin-left:", p1.x,
                            "px; width:", p2.x - p1.x, "px;'>&nbsp;</div>");
                } else {
                        str("<div class='", name, "' style='margin-left:", p1.x, "px;'>&nbsp;</div>");
                        if (props.line2 - props.line1 > 1)
                                str("<div class='", name, "' style='height:", p2.y - p1.y, "px'></div>");
                        str("<div class='", name, "' style='width:", p2.x, "px;'>&nbsp;</div>");
                }
                str("</div>");
                return str.get();
        };

        P._on_bufferOverlayChange = function(name, props, isNew) {
                if (!isNew) {
                        DOM.trash($(this.getOverlayId(name)));
                }
                var div = DOM.createFromHtml(this.getOverlayHTML(name, props));
                this.getElement().appendChild(div);
        };

        P._on_bufferOverlayDelete = function(name, props, isNew) {
                DOM.trash($(this.getOverlayId(name)));
        };

        /* -----[ self events ]----- */

        P._on_destroy = function() {
                this.setBuffer(null);
                this.__stopBlinking();
        };

        P._on_focus = function() {
                this.ymacs.setActiveFrame(this, true);
                this.buffer.cmd("goto_char", this.caretMarker.getPosition());
                this.buffer.addEventListener("onMessage", this._moreBufferEvents.onMessage);
        };

        P._on_blur = function() {
                this.caretMarker.setPosition(this.buffer.caretMarker.getPosition());
                this.buffer.removeEventListener("onMessage", this._moreBufferEvents.onMessage);
        };

        P._on_mouseDown = function(ev) {
                this.__restartBlinking();
                var pos = ev.computePos(this.getContentElement()),
                    sz = DOM.getOuterSize(this.getCaretElement()),
                    row = Math.floor(pos.y / sz.y),
                    col = Math.floor(pos.x / sz.x);
                // console.log("pos: %o, sz: %o, row: %o, col: %o", pos, sz, row, col);
                this.buffer._repositionCaret(this.buffer._rowColToPosition(row, col));
        };

        P._handle_focusKeys = function(ev) {
                if (this.buffer._handleKeyEvent(ev))
                        DlException.stopEventBubbling();
                return D.BASE._handle_focusKeys.apply(this, arguments);
        };

});

DEFINE_CLASS("Ymacs_Message_Popup", DlPopup, function(D, P) {
        D.FIXARGS = function(args) {
                args.focusable = false;
                args.autolink = false;
                args.zIndex = 5000;
        };
});
