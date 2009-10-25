// @require ymacs.js

DEFINE_CLASS("Ymacs_Frame", DlContainer, function(D, P, DOM) {

        D.DEFAULT_ARGS = {
                highlightCurrentLine : [ "highlightCurrentLine", true ],
                buffer               : [ "buffer", null ],

                // override in DlContainer
                _scrollBars           : [ "scroll"    , true ],

                // override in DlWidget
                _focusable           : [ "focusable"  , true ],
                _fillParent          : [ "fillParent" , true ]
        };

        var BLINK_TIMEOUT = 200;

        D.CONSTRUCT = function() {
                this.__blinkCaret = this.__blinkCaret.$(this);
                var tmp = this._bufferEvents = {
                        onLineChange    : this._on_bufferLineChange.$(this),
                        onInsertLine    : this._on_bufferInsertLine.$(this),
                        onDeleteLine    : this._on_bufferDeleteLine.$(this),
                        onPointChange   : this._on_bufferPointChange.$(this),
                        onResetCode     : this._on_bufferResetCode.$(this),
                        onOverwriteMode : this._on_bufferOverwriteMode.$(this)
                };
                tmp = this._moreBufferEvents = Object.makeCopy(tmp);
                tmp.onMessage = this._on_bufferMessage.$(this);
                var buffer = this.buffer;
                this.buffer = null;
                if (buffer)
                        this.setBuffer(buffer);
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
                        this.buffer.removeEventListener(this._moreBufferEvents);
                        if (this.buffer.activeFrame == this)
                                this.buffer.activeFrame = null;
                }
                this.buffer = buffer;
                if (buffer) {
                        buffer.addEventListener(this._bufferEvents);
                        this._redrawBuffer();
                        this._redrawCaret();
                }
        };

        P.centerOnCaret = function() {
                var row = this.buffer._rowcol.row,
                line = this.getLineDivElement(row),
                div = this.getElement();
                div.scrollTop = Math.round(line.offsetTop - div.clientHeight / 2 + line.offsetHeight / 2);
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

        P._redrawCaret = function() {
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
        };

        P._getLineHTML = function(row) {
                var line = this.buffer.code[row];
                if (line == "") {
                        line = "&nbsp;";
                } else if (this.buffer.tokenizer) {
                        line = this.buffer.tokenizer.highlightLine(row);
                } else {
                        line = line.htmlEscape();
                }
                return line;
        };

        P._redrawBuffer = function() {
                this.setContent(this.buffer.code.map(function(line, i){
                        return this._getLineHTML(i).htmlEmbed("div", "line");
                }, this).join(""));
        };

        P.heightInLines = function() {
                return Math.floor(this.getElement().clientHeight / this.getCaretElement().offsetHeight);
        };

        /* -----[ event handlers ]----- */

        P._on_bufferLineChange = function(row) {
                var div = this.getLineDivElement(row);
                div.innerHTML = this._getLineHTML(row);
        };

        P._on_bufferInsertLine = function(row) {
                var div = DOM.createFromHtml(this._getLineHTML(row).htmlEmbed("div", "line"));
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

        P._on_bufferMessage = function(type, text) {
                var popup = Ymacs_Message_Popup.get(0);
                popup.popup({
                        content : text.htmlEscape(),
                        widget  : this,
                        anchor  : this.getElement(),
                        align   : { prefer: "CC" }
                });
                popup.hide(2000);
        };

        P._on_destroy = function() {
                this.setBuffer(null);
                this.__stopBlinking();
        };

        P._on_focus = function() {
                this.buffer.activeFrame = this;
                this.buffer.addEventListener("onMessage", this._moreBufferEvents.onMessage);
        };

        P._on_blur = function() {
                this.buffer.activeFrame = null;
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
