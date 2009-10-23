DEFINE_CLASS("XCodeEditor", DlContainer, function(D, P, DOM){

        var BLINK_TIMEOUT = 200;
        var MAX_UNDO_RECORDS = 5000;
        var REQUEUE_REDO = 0;

        function POS(x) {
                return x instanceof XCodeEditor_Marker ? x.getPosition() : x;
        };

        function TEST_UNICODE_WORD_CHAR(c) {
                if (c) {
                        var code = c.charCodeAt(0);
                        return (code >= 48 && code <= 57) || c.toUpperCase() != c.toLowerCase();
                }
        };

        P.lastIndexOfRegexp = function(str, re, caret, bound) {
                if (bound == null || bound < 0)
                        bound = 0;
                var m, pos = 0, index = null;
                re.lastIndex = bound;
                re.global = true;
                this.matchData = null;
                while ((m = re.exec(str))) {
                        if (re.lastIndex > caret)
                                break;
                        this.matchData = m;
                        pos = re.lastIndex;
                        index = m.index;
                }
                return [ pos, index ];
        };

        D.newCommands = P.newCommands = function(cmds) {
                Object.merge(this.COMMANDS, cmds);
        };

        D.DEFAULT_ARGS = {
                _code       : [ "code"       , null ],

                // override in DlWidget
                _focusable  : [ "focusable"  , true ],
                _fillParent : [ "fillParent" , true ]
        };

        D.FIXARGS = function(args) {
                args.scroll = true;
                if (args.code == null)
                        args.code = "";
        };

        D.CONSTRUCT = function() {
                this.COMMANDS = Object.makeCopy(D.COMMANDS);
                this.markers = [];
                this.__blinkCaret = this.__blinkCaret.$(this);
                this.__savingExcursion = 0;
                this.__preventUpdates = 0;
                this.__preventUndo = 0;
                this.__undoQueue = [];
                this.__redoQueue = [];
                this.__undoInProgress = 0;
                this.caretMarker = this.createMarker(0, false, "point");
                this.markMarker = this.createMarker(0, true, "mark");
                this.matchData = [];
                this.fillColumn = 78;
                this.tabWidth = 8;
                this.previousCommand = null;
                this.currentCommand = null;
                this.inInteractiveCommand = 0;
                this.variables = {
                        case_fold_search: true
                };
                this.caretMarker.onChange = function(pos) {
                        var editor = this.editor;
                        editor._rowcol = editor.caretMarker.getRowCol();
                        // XXX: this shouldn't be needed
                        if (editor.inInteractiveCommand == 0 && editor.__savingExcursion == 0)
                                editor._redrawCaret();
                };
                this.syntax = {
                        word_ng       : { test: TEST_UNICODE_WORD_CHAR },
                        paragraph_sep : /\n\s*\n/g
                };
                this.keymap = [];
                this._keymap_isearch = new XCodeEditor_Keymap_ISearch({ editor: this });
                this.pushKeymap(this.makeDefaultKeymap());
                this.setCode(this._code);
                this.killRing = [];
                this.killMasterOfRings = [];
                this._lastCommandWasKill = 0;
                delete this["_code"];
        };

        P.initDOM = function() {
                D.BASE.initDOM.apply(this, arguments);
                this.getElement().innerHTML = "<div class='content'></div><div class='XCodeEditor-caret'>&nbsp;</div>";
                this.addEventListener({
                        onDestroy   : this._on_destroy.$(this),
                        onFocus     : this._on_focus.$(this),
                        onBlur      : this._on_blur.$(this),
                        onMouseDown : this._on_mouseDown.$(this)
                });
        };

        /* -----[ dynamic variables ]----- */

        // Who said dynamic scope is bad?  Ever since I'm using Lisp I
        // started considering them one of the most valuable features.
        // Everybody is using dynamic scope.
        //
        // Since we don't have real dynamic scope in JS, we store the
        // values in a hash and using the withVariables method we can
        // assign temporary values to them and execute a function.

        P.withVariables = function(vars, cont) {
                var saved = {}, i, ret;
                for (i in vars) {
                        saved[i] = this.variables[i];
                        this.variables[i] = vars[i];
                }
                try {
                        return cont.apply(this, Array.$(arguments, 2));
                } finally {
                        for (i in saved)
                                this.variables[i] = saved[i];
                }
        };

        P.getVariable = function(key) {
                return this.variables[key];
        };

        P.setVariable = function(key, val) {
                return this.variables[key] = val;
        };

        P.setq = P.setVariable;
        P.getq = P.getVariable;

        /* -----[ public API ]----- */

        P.getContentElement = function() {
                return this.getElement().firstChild;
        };

        P.getCaretElement = function() {
                return this.getElement().childNodes[1];
        };

        P.pushKeymap = function(keymap) {
                this.popKeymap(keymap);
                this.keymap.push(keymap);
        };

        P.popKeymap = function(keymap) {
                this.keymap.remove(keymap);
        };

        P.makeDefaultKeymap = function() {
                return new XCodeEditor_Keymap_Emacs({ editor: this });
        };

        P.signalError = function(text) {
                // XMSG.addMsg("error", text);
                if (window.console)
                        console.log("ERROR: %s", text);
        };

        P.signalInfo = function(text) {
                // XMSG.addMsg("info", text);
                if (window.console)
                        console.log("INFO: %s", text);
        };

        P.createMarker = function(pos, before, name) {
                return new XCodeEditor_Marker({ editor: this, pos: pos, name: name, before: before });
        };

        P.point = function() {
                return this.caretMarker.getPosition();
        };

        P.setCode = function(code) {
                this.__code = code = code.replace(/\t/g, " ".x(this.tabWidth));
                this.__size = code.length;
                this.__undoQueue = [];
                this.__redoQueue = [];
                this.markers = [ this.caretMarker, this.markMarker ]; // resetting the code invalidates markers
                this.code = code.split(/\n/);
                this._redrawBuffer();
                this.caretMarker.setPosition(0, false, true);
                this.markMarker.setPosition(0, true);
        };

        P.getCode = function() {
                return this.__code || (this.__code = this.code.join("\n"));
        };

        P.charAtRowCol = function(row, col) {
                var a = this.code[row];
                if (!a)
                        return null;
                if (col == a.length)
                        return row == this.code.length - 1 ? a[col] : "\n";
                return a[col];
        };

        P.charAt = function(point) {
                if (point == null)
                        point = this.point();
                else {
                        point = POS(point);
                        if (point < 0)
                                point += this.point();
                }
                var rc = this._positionToRowCol(point);
                return this.charAtRowCol(rc.row, rc.col);
        };

        P.getLineDivElement = function(row) {
                return this.getContentElement().childNodes[row];
        };

        P.getCodeSize = function() {
                if (this.__size)
                        return this.__size;
                var size = 0;
                this.code.foreach(function(line){
                        size += line.length + 1;
                });
                if (size > 0)
                        size--;
                return this.__size = size;
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

        P.makeInteractiveHandler = function(func, cmd, args) {
                return function() {
                        this.currentCommand = cmd;
                        // the amount of brain twisting to get
                        // this right is incredible. :-(  I give up.
                        if (cmd != "undo") {
                                this.__undoQueue = this.__undoQueue.concat(this.__redoQueue);
                                this.__redoQueue = [];
                        }
                        if (this.previousCommand != cmd) {
                                this.sameCommandCount = 0;
                                if (cmd != "undo") {
                                        this._placeUndoBoundary();
                                }
                        } else if (cmd != "self_insert_command" || this.sameCommandCount % 20 == 0) {
                                if (cmd != "undo") {
                                        this._placeUndoBoundary();
                                }
                        }
                        ++this.inInteractiveCommand;
                        var ret = func.apply(this, arguments);
                        --this.inInteractiveCommand;
                        this.previousCommand = cmd;
                        this.sameCommandCount++;
                        this.ensureCaretVisible();
                        return ret;
                }.$A(this, args);
        };

        P.resetOverwriteMode = function(om) {
                if (arguments.length == 0)
                        om = this.overwriteMode;
                this.condClass(this.overwriteMode = !om, "XCodeEditor-overwrite-mode");
                this.signalInfo(!om ? "Replace mode" : "Insert mode");
        };

        P.setMinibuffer = function(text) {
                if (this.minibuffer) {
                        this.minibuffer.setCode(text);
                        this.minibuffer.cmd("end_of_buffer");
                }
        };

        P.cmd = function(cmd) {
                var ret = this.COMMANDS[cmd].apply(this, Array.$(arguments, 1));
                return ret;
        };

        P.cmdRepeat = function(times) {
                var args = Array.$(arguments, 1);
                while (times-- > 0)
                        this.cmd.apply(this, args);
        };

        /* -----[ not-so-public API ]----- */

        // BEGIN: undo queue

        // DEL operations save the removed text.
        // ADD operations only save the amount of added text.

        P._recordChange = function(type, pos, len, text) {
                if (len > 0) {
                        var q = this.__undoQueue;
                        q.push({
                                type  : type,
                                pos   : pos,
                                len   : len,
                                text  : text
                        });
                        if (q.length > MAX_UNDO_RECORDS)
                                q.shift();
                }
        };

        P._placeUndoBoundary = function(q) {
                q = q || this.__undoQueue;
                var m = this.markers.map(function(m){
                        return [ m, m.getPosition() ];
                });
                var last = q.peek();
                if (!last || last.type != 3) {
                        q.push({ type: 3, markers: m });
                } else {
                        last.markers = m;
                }
        };

        P._playbackUndo = function(q) {
                ++this.__undoInProgress;
                var didit = false, action;
                while (q.length > 0 && q.peek().type == 3) {
                        action = q.pop();
                }
                while (q.length > 0) {
                        action = q.pop();
                        if (action.type == 3) { // boundary
                                // restore markers
                                action.markers.foreach(function(m){
                                        m[0].setPosition(m[1]);
                                });
                                break;
                        }
                        didit = true;
                        var pos = action.pos;
                        switch (action.type) {
                            case 1: // insert
                                this._deleteText(pos, pos + action.len);
                                break;
                            case 2: // delete
                                this._insertText(action.text, pos);
                                break;
                        }
                }
                --this.__undoInProgress;
                return didit;
        };

        // END: undo

        P._replaceLine = function(row, text) {
                this.code[row] = text;
                if (this.__preventUpdates == 0) {
                        if (text == "")
                                text = "&nbsp;";
                        else
                                text = text.htmlEscape();
                        var div = this.getLineDivElement(row);
                        div.innerHTML = text;
                }
        };

        P._deleteLine = function(row) {
                this.code.splice(row, 1);
                if (this.__preventUpdates == 0) {
                        DOM.trash(this.getLineDivElement(row));
                }
        };

        P._insertLine = function(row, text) {
                this.code.splice(row, 0, text);
                if (this.__preventUpdates == 0) {
                        var div = DOM.createElement("div", null, { className: "line" });
                        if (text == "")
                                text = "&nbsp;";
                        else
                                text = text.htmlEscape();
                        div.innerHTML = text;
                        this.getContentElement().insertBefore(div, this.getLineDivElement(row));
                }
        };

        P._insertText = function(text, pos) {
                if (pos == null)
                        pos = this.caretMarker.getPosition();
                pos = POS(pos);
                // *** UNDO RECORDING
                if (this.__preventUndo == 0)
                        this._recordChange(1, pos, text.length);
                var rc = this._positionToRowCol(pos);
                var lines = text.split(/\n/), i = rc.row, rest = this.code[i].substr(rc.col);
                this._replaceLine(i, this.code[i].substr(0, rc.col) + lines.shift());
                lines.foreach(function(text){
                        this._insertLine(++i, text);
                }, this);
                this._replaceLine(i, this.code[i] + rest);
                this._updateMarkers(pos, text.length);
        };

        P._deleteText = function(begin, end) {
                begin = POS(begin);
                end = POS(end);
                if (end < begin) { var tmp = begin; begin = end; end = tmp; }
                // *** UNDO RECORDING
                if (this.__preventUndo == 0)
                        this._recordChange(2, begin, end - begin, this._bufferSubstring(begin, end));
                var brc = this._positionToRowCol(begin),
                    erc = this._positionToRowCol(end);
                var line = this.code[brc.row];
                if (brc.row == erc.row) {
                        // same line, that's easy
                        line = line.substr(0, brc.col) + line.substr(erc.col);
                        this._replaceLine(brc.row, line);
                } else {
                        // fix first line
                        line = line.substr(0, brc.col) + this.code[erc.row].substr(erc.col);
                        this._replaceLine(brc.row, line);
                        // delete lines in between
                        line = brc.row + 1;
                        (erc.row - brc.row).times(this._deleteLine.$(this, line));
                }
                this._updateMarkers(begin, begin - end, begin);
        };

        P._replaceText = function(begin, end, text) {
                this._deleteText(begin, end);
                this._insertText(text, begin);
        };

        P._swapAreas = function(a) {
                a = a.map(POS).mergeSort();
                var b1 = a[0],
                    e1 = a[1],
                    b2 = a[2],
                    e2 = a[3],
                    t1 = this._bufferSubstring(b1, e1),
                    t2 = this._bufferSubstring(b2, e2);
                this._replaceText(b2, e2, t1);
                this._replaceText(b1, e1, t2);
                return e2;
        };

        P._bufferSubstring = function(begin, end) {
                if (begin == null) begin = this.point();
                else begin = POS(begin);

                if (end == null) end = this.getCodeSize();
                else end = POS(end);

                if (end < begin) { var tmp = begin; begin = end; end = tmp; }
                // var brc = this._positionToRowCol(begin),
                //     erc = this._positionToRowCol(end);
                // if (brc.row == erc.row) {
                //         return this.code[brc.row].substring(brc.col, erc.col);
                // } else return [ this.code[brc.row].substr(brc.col) ].
                //         concat(this.code.slice(brc.row + 1, erc.row)).
                //         concat(this.code[erc.row].substr(0, erc.col)).
                //         join("\n");
                return this.getCode().substring(begin, end);
        };

        P._killingAction = function(p1, p2, prepend, noDelete) {
                p1 = POS(p1);
                p2 = POS(p2);
                var text = this._bufferSubstring(p1, p2);
                if (!this._lastCommandWasKill) {
                        if (this.killRing.length && (this.killMasterOfRings.length == 0 ||
                                                     this.killMasterOfRings.peek().join("") != this.killRing.join("")))
                                this.killMasterOfRings.push(this.killRing);
                        this.killRing = [];
                }
                prepend ? this.killRing.unshift(text)
                        : this.killRing.push(text);
                if (!noDelete)
                        this._deleteText(p1, p2);
                this._lastCommandWasKill++;
        };

        P._positionToRowCol = function(pos) {
                var line = 0;
                while (pos > 0 && line < this.code.length) {
                        var len = this.code[line].length;
                        if (len >= pos)
                                break;
                        pos -= len + 1; // one for the newline
                        line++;
                }
                return { row: line, col: pos };
        };

        P._rowColToPosition = function(row, col) {
                var pos = 0, i = 0;
                while (i < row && i < (this.code.length - 1))
                        pos += this.code[i++].length + 1; // one for the newline
                pos += Math.min(col, this.code[i].length);
                return pos;
        };

        P._boundPosition = function(pos) {
                if (pos < 0)
                        return 0;
                return Math.min(pos, this.getCodeSize());
        };

        P._redrawBuffer = function() {
                this.setContent(this.code.map(function(line){
                        if (line == "")
                                line = "&nbsp;";
                        else
                                line = line.htmlEscape();
                        return line.htmlEmbed("div", "line");
                }).join(""));
        };

        P._repositionCaret = function(pos) {
                var p = this.caretMarker.getPosition();
                if (pos == null)
                        pos = p;
                pos = POS(pos);
                pos = this._boundPosition(pos);
                this.caretMarker.setPosition(pos);
                return pos != p;
        };

        P._redrawCaret = function() {
                var rc = this._rowcol, caret = this.getCaretElement(), w = caret.offsetWidth, h = caret.offsetHeight;
                caret.style.left = (w * rc.col) + "px";
                caret.style.top = (h * rc.row) + "px";
                this.__restartBlinking();
                var ch = this.charAtRowCol(rc.row, rc.col);
                if (ch == "\n" || !ch)
                        ch = "&nbsp;";
                this.getCaretElement().innerHTML = ch;
                if (this.__hoverLine != null)
                        DOM.delClass(this.getLineDivElement(this.__hoverLine), "XCodeEditor-current-line");
                DOM.addClass(this.getLineDivElement(rc.row), "XCodeEditor-current-line");
                this.__hoverLine = rc.row;
        };

        P._updateMarkers = function(offset, delta, min) {
                this.__size = null;
                this.__code = null;
                if (this.__undoInProgress == 0)
                        this.markers.map("editorChange", offset, delta, min || 0);
        };

        P._saveExcursion = function(cont, preventUpdates) {
                var tmp = this.createMarker(this.point());
                ++this.__savingExcursion;
                if (preventUpdates)
                        ++this.__preventUpdates;
                var ret;
                try {
                        return cont.call(this, tmp.getPosition());
                } finally {
                        --this.__savingExcursion;
                        if (preventUpdates)
                                if (--this.__preventUpdates == 0)
                                        this._redrawBuffer();
                        this.caretMarker.swap(tmp, false, true);
                        tmp.destroy();
                }
        };

        P._heightInLines = function() {
                return Math.floor(this.getElement().clientHeight / this.getCaretElement().offsetHeight);
        };

        /* -----[ event handlers ]----- */

        P._on_destroy = function() {
                this.__stopBlinking();
        };

        P._on_focus = function() {
        };

        P._on_blur = function() {
        };

        P._on_mouseDown = function(ev) {
                this.__restartBlinking();
                var pos = ev.computePos(this.getContentElement()),
                    sz = DOM.getOuterSize(this.getCaretElement()),
                    row = Math.floor(pos.y / sz.y),
                    col = Math.floor(pos.x / sz.x);
                // console.log("pos: %o, sz: %o, row: %o, col: %o", pos, sz, row, col);
                this._repositionCaret(this._rowColToPosition(row, col));
        };

        P._handle_focusKeys = function(ev) {
                var handled = false;
                this.interactiveEvent = ev;
                var lcwk = this._lastCommandWasKill;

                this.keymap.r_foreach(function(km){
                        handled = km.handleKeyEvent(ev);
                        if (handled)
                                $BREAK();
                });

                if (this._lastCommandWasKill == lcwk && typeof handled != "object") // selecting a prefix keymap shouldn't clear the killRing
                        this._lastCommandWasKill = 0;

                this.interactiveEvent = null;
                if (handled)
                        DlException.stopEventBubbling();
                return D.BASE._handle_focusKeys.apply(this, arguments);
        };

        P.__restartBlinking = function() {
                DOM.delClass(this.getCaretElement(), "XCodeEditor-caret-blink");
                this.__stopBlinking();
                this.__caretTimer = setInterval(this.__blinkCaret, BLINK_TIMEOUT);
        };

        P.__stopBlinking = function() {
                clearInterval(this.__caretTimer);
        };

        P.__blinkCaret = function() {
                DOM.condClass(this.getCaretElement(), this.BLINKING =! this.BLINKING, "XCodeEditor-caret-blink");
        };

}).COMMANDS = {

        forward_char: function(x) {
                if (x == null) x = 1;
                return this._repositionCaret(this.point() + x);
        },

        backward_char: function(x) {
                if (x == null) x = 1;
                return this.cmd("forward_char", -x);
        },

        forward_line: function(x) {
                if (x == null) x = 1;
                var rc = this._rowcol;
                return this._repositionCaret(this._rowColToPosition(rc.row + x, rc.col));
        },

        backward_line: function(x) {
                if (x == null) x = 1;
                return this.cmd("forward_line", -x);
        },

        forward_whitespace: function() {
                if (this.cmd("search_forward_regexp", /[^\s]/g))
                        this.cmd("backward_char");
        },

        backward_whitespace: function() {
                if (this.cmd("search_backward_regexp", /[^\s]/g))
                        this.cmd("forward_char");
        },

        beginning_of_line: function() {
                var rc = this._rowcol;
                return this._repositionCaret(this._rowColToPosition(rc.row, 0));
        },

        back_to_indentation: function() {
                var rc = this._rowcol, line = this.code[rc.row], m = /\S/.exec(line);
                if (m)
                        return this._repositionCaret(this._rowColToPosition(rc.row, m.index));
        },

        beginning_of_indentation_or_line: function() {
                return this.cmd("back_to_indentation") || this.cmd("beginning_of_line");
        },

        end_of_line: function() {
                var rc = this._rowcol;
                return this._repositionCaret(this._rowColToPosition(rc.row, this.code[rc.row].length));
        },

        beginning_of_buffer: function() {
                return this._repositionCaret(0);
        },

        end_of_buffer: function() {
                return this._repositionCaret(this.getCodeSize());
        },

        backward_delete_char: function() {
                var pos = this.point();
                if (pos > 0) {
                        var rc = this._rowcol, line = this.code[rc.row], ch;
                        if (rc.col > 0) {
                                ch = line.charAt(rc.col - 1);
                                line = line.substr(0, rc.col - 1) + line.substr(rc.col);
                                this._replaceLine(rc.row, line);
                        } else {
                                ch = "\n";
                                // merge with the previous line
                                line = this.code[rc.row - 1] + line;
                                this._replaceLine(rc.row - 1, line);
                                this._deleteLine(rc.row);
                        }
                        // *** UNDO RECORDING
                        if (!this.__preventUndo)
                                this._recordChange(2, pos - 1, 1, ch);
                        this._updateMarkers(pos, -1);
                }
        },

        delete_char: function() {
                if (this.cmd("forward_char"))
                        this.cmd("backward_delete_char");
        },

        delete_whitespace: function() {
                var ret = false;
                while (/^\s$/.test(this.charAt())) {
                        ret = true;
                        this.cmd("delete_char");
                }
                return ret;
        },

        backward_delete_whitespace: function() {
                var ret = false;
                while (/^\s$/.test(this.charAt(-1))) {
                        ret = true;
                        this.cmd("backward_delete_char");
                }
                return ret;
        },

        overwrite_mode: function() {
                this.resetOverwriteMode();
        },

        self_insert_command: function(ev) {
                if (!ev)
                        ev = this.interactiveEvent;
                var ch = String.fromCharCode(ev.charCode),
                    rc = this._rowcol;
                if (ev.charCode && ch && !ev.altKey && !ev.ctrlKey) {
                        var line = this.code[rc.row];
                        // *** UNDO RECORDING
                        if (!this.__preventUndo) {
                                if (this.overwriteMode && rc.col < line.length)
                                        this._recordChange(2, this.point(), 1, line.charAt(rc.col));
                                this._recordChange(1, this.point(), 1);
                        }
                        line = line.substr(0, rc.col) + ch + line.substr(this.overwriteMode ? rc.col + 1 : rc.col);
                        this._replaceLine(rc.row, line);
                        this.caretMarker.updateMarkers(+1);
                        ev.domStop = true;
                        return true;
                }
                return false;
        },

        newline: function() {
                var rc = this._rowcol,
                    line = this.code[rc.row],
                    rest = line.substr(rc.col);
                line = line.substr(0, rc.col);
                this._replaceLine(rc.row, line);
                this._insertLine(rc.row + 1, rest);
                // *** UNDO RECORDING
                if (!this.__preventUndo)
                        this._recordChange(1, this.point(), 1);
                this.caretMarker.updateMarkers(+1);
        },

        indent_line: function() {
                this.signalError("Heh, you wish!");
        },

        insert_text: function(txt) {
                return this._insertText(txt);
        },

        make_marker: function(pos) {
                return this.createMarker(pos);
        },

        looking_at: function(rx) {
                rx.global = true;
                var pos = rx.lastIndex = this.point();
                var ret = this.matchData = rx.exec(this.getCode());
                // console.log(ret, ret && ret.index);
                return ret && ret.index == pos;
        },

        looking_back: function(rx, bound) {
                rx.global = true;
                if (bound < 0)
                        bound += this.point();
                var pos = this.point();
                var index = this.lastIndexOfRegexp(this.getCode(), rx, pos, bound)[0];
                return pos == index;
        },

        search_forward: function(str) {
                var code = this.getCode(), point = this.point();
                if (this.getq("case_fold_search")) {
                        code = code.toLowerCase();
                        str = str.toLowerCase();
                }
                var pos = code.indexOf(str, point);
                if (pos >= 0) {
                        this._repositionCaret(pos + str.length);
                        return true;
                }
        },

        search_backward: function(str) {
                var code = this.getCode(), point = this.point();
                if (this.getq("case_fold_search")) {
                        code = code.toLowerCase();
                        str = str.toLowerCase();
                }
                var pos = code.lastIndexOf(str, point);
                if (pos == point)
                        pos = code.lastIndexOf(str, point - 1);
                if (pos >= 0 && pos != point) {
                        this._repositionCaret(pos);
                        return true;
                }
        },

        search_forward_regexp: function(rx) {
                rx.global = true;
                var code = this.getCode();
                var pos = rx.lastIndex = this.point();
                var ret = this.matchData = rx.exec(code);
                if (ret && rx.lastIndex != pos) {
                        this._repositionCaret(rx.lastIndex);
                        return true;
                }
        },

        search_backward_regexp: function(rx) {
                rx.global = true;
                var pos = this.point();
                var index = this.lastIndexOfRegexp(this.getCode(), rx, pos)[1];
                if (index != null && index != pos) {
                        this._repositionCaret(index);
                        return true;
                }
        },

        forward_word: function(x) {
                if (x == null) x = 1;
                var word = this.syntax.word_ng, end = false;
                while (!end && !word.test(this.charAt()))
                        if (!this.cmd("forward_char", 1))
                                end = true;
                while (!end && word.test(this.charAt()))
                        if (!this.cmd("forward_char", 1))
                                end = true;
        },

        backward_word: function(x) {
                if (x == null) x = 1;
                var word = this.syntax.word_ng, end = false;
                while (!end && !word.test(this.charAt(-1)))
                        if (!this.cmd("backward_char", 1))
                                end = true;
                while (!end && word.test(this.charAt(-1)))
                        if (!this.cmd("backward_char", 1))
                                end = true;
        },

        forward_paragraph: function(x) {
                this.cmd("forward_whitespace");
                if (this.cmd("search_forward_regexp", this.syntax.paragraph_sep))
                        this.cmd("goto_char", this.cmd("match_beginning") + 1);
                else
                        this.cmd("end_of_buffer");
        },

        backward_paragraph: function(x) {
                this.cmd("backward_whitespace");
                if (this.cmd("search_backward_regexp", this.syntax.paragraph_sep))
                        this.cmd("goto_char", this.cmd("match_end") - 1);
                else
                        this.cmd("beginning_of_buffer");
        },

        transpose_words: function() {
                // if we're in the middle of a word, some
                // weird things happen; better skip it, just
                // like Emacs does.
                this.cmd("backward_char");
                if (this.syntax.word_ng.test(this.charAt()))
                        this.cmd("forward_word");

                // save next word position
                var wp1 = this.cmd("save_excursion", function(){
                        this.cmd("forward_word");
                        var p1 = this.point();
                        this.cmd("backward_word");
                        return [ p1, this.point() ];
                });

                // save previous word position
                var wp2 = this.cmd("save_excursion", function(){
                        this.cmd("backward_word");
                        var p1 = this.point();
                        this.cmd("forward_word");
                        return [ p1, this.point() ];
                });

                this.cmd("goto_char", this._swapAreas(wp1.concat(wp2)));
        },

        transpose_chars: function() {
                var pos = this.point();
                if (this.cmd("backward_char"))
                        this.cmd("goto_char", this._swapAreas([ pos - 1, pos, pos, pos + 1 ]));
        },

        kill_word: function() {
                var pos = this.point();
                this.cmd("forward_word");
                var pos2 = this.point();
                this._killingAction(pos, pos2, false);
        },

        backward_kill_word: function() {
                var pos = this.point();
                this.cmd("backward_word");
                var pos2 = this.point();
                this._killingAction(pos, pos2, true);
        },

        goto_char: function(pos) {
                return this._repositionCaret(pos);
        },

        insert: function(text) {
                return this._insertText(text);
        },

        kill_line: function(x, wholeLine) {
                if (typeof x == "number" && x != 0)
                        this.cmdRepeat(x, "kill_line", 0, true);
                var pos = this.point(),
                rc = this._rowcol,
                line = this.code[rc.row],
                end = pos + line.length - rc.col;
                if (rc.row < this.code.length - 1 && (wholeLine || this.cmd("looking_at", /\s*$/mg)))
                        end++;
                this._killingAction(pos, end);
        },

        save_excursion: function() {
                return this._saveExcursion.apply(this, arguments);
        },

        point: function() {
                return this.caretMarker.getPosition();
        },

        kill_region: function() {
                this._killingAction(this.caretMarker, this.markMarker);
        },

        copy_region_as_kill: function() {
                this._killingAction(this.caretMarker, this.markMarker, false, true);
        },

        yank: function() {
                var point = this.point();
                this._insertText(this.killRing.join(""));
                this.cmd("set_mark_command", point);
        },

        yank_pop: function() {
                if (/^yank/.test(this.previousCommand) && this.killMasterOfRings.length > 0) {
                        this.killMasterOfRings.unshift(this.killRing);
                        this.killRing = this.killMasterOfRings.pop();
                        this._deleteText(this.caretMarker, this.markMarker);
                        this.cmd("yank");
                } else {
                        this.signalError("Previous command was not a yank");
                }
        },

        yank_shift: function() {
                if (/^yank/.test(this.previousCommand) && this.killMasterOfRings.length > 0) {
                        this.killMasterOfRings.push(this.killRing);
                        this.killRing = this.killMasterOfRings.shift();
                        this._deleteText(this.caretMarker, this.markMarker);
                        this.cmd("yank");
                } else {
                        this.signalError("Previous command was not a yank");
                }
        },

        mark: function() {
                return this.markMarker.getPosition();
        },

        set_mark_command: function(x) {
                if (x == null) x = this.point();
                this.markMarker.setPosition(x);
        },

        exchange_point_and_mark: function() {
                this.caretMarker.swap(this.markMarker);
                this.cmd("recenter_top_bottom");
        },

        recenter_top_bottom: function() {
                var row = this._rowcol.row,
                line = this.getLineDivElement(row),
                div = this.getElement();
                div.scrollTop = Math.round(line.offsetTop - div.clientHeight / 2 + line.offsetHeight / 2);
        },

        _apply_operation_on_word: function (op) {
                var pos = this.point();
                if (this.syntax.word_ng.test(this.charAt())) {
                        var pos2 = this.cmd("save_excursion", function(){
                                this.cmd("forward_word");
                                return this.point();
                        });
                        var word = op.call(this._bufferSubstring(pos, pos2));
                        this._deleteText(pos, pos2);
                        this._insertText(word);
                } else {
                        this.cmd("forward_word");
                        this.cmd("backward_word");
                        if (pos != this.point())
                                this.cmd(this.currentCommand);
                }
        },

        capitalize_word: function() {
                this.cmd("_apply_operation_on_word", function() {
                        return this.charAt(0).toUpperCase() + this.substr(1).toLowerCase();
                });
        },

        downcase_word: function() {
                this.cmd("_apply_operation_on_word", String.prototype.toLowerCase);
        },

        upcase_word: function() {
                this.cmd("_apply_operation_on_word", String.prototype.toUpperCase);
        },

        fill_paragraph: function() {
                this.cmd("save_excursion", function(){
                        if (!this.cmd("looking_at", this.syntax.paragraph_sep))
                                this.cmd("forward_paragraph");
                        var eop = this.createMarker(this.point() - 1);
                        this.cmd("backward_paragraph");
                        this.cmd("forward_char");

                        // remove newlines first
                        // console.time("newlines");
                        while (true) {
                                this.cmd("end_of_line");
                                this.cmd("backward_delete_whitespace");
                                if (this.point() >= eop.getPosition())
                                        break;
                                this._replaceText(this.point(), this.point() + 1, " ");
                        }
                        // console.timeEnd("newlines");

                        this.cmd("beginning_of_line");

                        // main operation
                        // console.time("filling");
                        while (this.point() < eop.getPosition()) {
                                this.cmd("forward_word");
                                if (this._rowcol.col > this.fillColumn) {
                                        this.cmd("backward_word");
                                        this.cmd("backward_delete_whitespace");
                                        this.cmd("newline");
                                }
                        }
                        // console.timeEnd("filling");

                        eop.destroy();
                });
        },

        scroll_down: function() {
                var hl = this._heightInLines();
                this.cmd("forward_line", Math.round(hl / 1.33));
                this.cmd("recenter_top_bottom");
        },

        scroll_up: function() {
                var hl = this._heightInLines();
                this.cmd("backward_line", Math.round(hl / 1.33));
                this.cmd("recenter_top_bottom");
        },

        nuke_trailing_whitespace: function() {
                this.cmd("save_excursion", function(){
                        this.cmd("goto_char", 0);
                        while (this._rowcol.row < this.code.length) {
                                var line = this.code[this._rowcol.row],
                                m = /\s+$/.exec(line);
                                if (m)
                                        this._deleteText(this.point() + m.index, this.point() + line.length);
                                if (!this.cmd("forward_line"))
                                        break;
                        }
                }, true);
        },

        match_string: function(n) {
                return this.matchData[n];
        },

        match_beginning: function() {
                return this.matchData.index;
        },

        match_end: function() {
                return this.matchData.index + this.matchData[0].length;
        },

        undo: function() {
                var q = this.__undoQueue;
                this.__undoQueue = this.__redoQueue;
                this._placeUndoBoundary();
                if (!this._playbackUndo(q)) {
                        this.signalError("No further undo information");
                }
                this.__undoQueue = q;
        },

        center_line: function() {
                this.cmd("save_excursion", function(){
                        this.cmd("end_of_line");
                        this.cmd("backward_delete_whitespace");
                        this.cmd("beginning_of_line");
                        this.cmd("delete_whitespace");
                        var line = this.code[this._rowcol.row];
                        var indent = Math.floor((this.fillColumn - line.length) / 2);
                        this.cmd("insert", " ".x(indent));
                });
        },

        delete_region_or_line: function() {
                // right now this just deletes the line, since there's
                // no notion of transient region
                this.cmd("beginning_of_line");
                var pos = this.point();
                if (this.cmd("forward_line")) {
                        this._deleteText(pos, this.point());
                        return true;
                }
        },

        bind_variables: function() {
                return this.withVariables.apply(this, arguments);
        }

};

/* -----[ commands to help using the system clipboard ]----- */

XCodeEditor.newCommands((function(){

        function modalTextarea(title, text, cont) {
                var dlg = new DlDialog({ parent  : this.getParentDialog() || this,
                                         title   : title,
                                         quitBtn : "destroy",
                                         modal   : true });
                var entry = new DlEntry({ parent: dlg, type: "textarea", fillParent: true, value: text });
                dlg._focusedWidget = entry;
                dlg.setSize({ x: 350, y: 250 });
                entry.addEventListener("onKeyPress", function(ev){
                        if (ev.keyCode != DlKeyboard.ESCAPE) {
                                dlg.destroy();
                                cont.call(this, entry);
                        }
                }.clearingTimeout(2, this));
                dlg.show(true);
                dlg.addEventListener("onDestroy", this.focus.clearingTimeout(2, this));
                entry.select();
        };

        return {

                yank_from_operating_system: function() {
                        modalTextarea.call(this, "Paste below (press CTRL-V)", null, function(entry){
                                this.cmd("insert", entry.getValue());
                        });
                },

                copy_for_operating_system: function() {
                        var text = this._bufferSubstring(this.caretMarker, this.markMarker);
                        modalTextarea.call(this, "Press CTRL-C", text, function(entry){
                                this.cmd("copy_region_as_kill");
                        });
                },

                kill_for_operating_system: function() {
                        var text = this._bufferSubstring(this.caretMarker, this.markMarker);
                        modalTextarea.call(this, "Press CTRL-C or CTRL-X", text, function(entry){
                                this.cmd("kill_region");
                        });
                }

        };

})());

/* -----[ markers are objects that hold a position which is
   automatically maintained as text is inserted or
   removed ]----- */

DEFINE_CLASS("XCodeEditor_Marker", null, function(D, P){

        D.DEFAULT_ARGS = {
                position : [ "pos"    , null ],
                editor   : [ "editor" , null ],
                before   : [ "before" , false ],
                name     : [ "name"   , null ]
        };

        D.CONSTRUCT = function() {
                this.editor.markers.push(this);
        };

        P.onChange = Function.noop;

        P.destroy = function() {
                this.editor.markers.remove(this);
                this.editor = null;
        };

        P.editorChange = function(pos, diff, min) {
                var p = this.position;
                if (this.before)
                        --p;
                if (diff != 0 && pos <= p) {
                        this.position += diff;
                        if (this.position < min)
                                this.position = min;
                        this.onChange(this.position);
                }
        };

        P.getPosition = function() {
                return this.position;
        };

        P.setPosition = function(pos, noHooks, force) {
                if (force || this.position != pos) {
                        this.position = pos;
                        if (!noHooks)
                                this.onChange(this.position);
                }
        };

        P.getRowCol = function() {
                return this.editor._positionToRowCol(this.position);
        };

        P.updateMarkers = function(delta) {
                this.editor._updateMarkers(this.getPosition(), delta);
        };

        P.swap = function(other, noHooks, force) {
                var tmp = this.getPosition();
                this.setPosition(other.getPosition(), noHooks, force);
                other.setPosition(tmp, noHooks, force);
        };

});

DEFINE_CLASS("XCodeEditor_Keymap", null, function(D, P){

        var REVERSE_KEYS = {};
        Object.foreach(DlKeyboard, function(val, key) {
                if (typeof val == "number")
                        REVERSE_KEYS[val] = key;
        });

        D.DEFAULT_ARGS = {
                definitions : [ "definitions" , null ],
                editor      : [ "editor"      , null ]
        };

        D.FIXARGS = function(args) {
                if (!args.definitions)
                        args.definitions = {};
        };

        D.CONSTRUCT = function() {
                this.defaultHandler = this.makeHandler(this.editor.COMMANDS["self_insert_command"], "self_insert_command");
                this.currentPrefix = null;
                this.currentKeys = [];
        };

        P.parseKey = function(str) {
                var key = {};
                var a = str.split(/-/);
                a.reverse();
                a.foreach(function(c, i){
                        if (i == 0) {
                                if (typeof DlKeyboard[c] == "number")
                                        key.keyCode = DlKeyboard[c];
                                else {
                                        a[i] = c.toLowerCase();
                                        key.charCode = a[i].charCodeAt(0);
                                }
                        } else switch(c) {
                            case "C": key.ctrlKey = true; break;
                            case "M": key.metaKey = true; break;
                            case "S": key.shiftKey = true; break;
                        }
                });
                a.reverse();
                var c = a.pop();
                key.str = a.sort().join("-");
                if (key.str)
                        key.str += "-";
                key.str += c;
                return key;
        };

        P.unparseKey = function(ev) {
                var key, modifiers = [];
                if (ev.keyCode in REVERSE_KEYS)
                        key = REVERSE_KEYS[ev.keyCode];
                else if (ev.charCode) {
                        if (ev.charCode == 32)
                                key = "SPACE";
                        else
                                key = String.fromCharCode(ev.charCode).toLowerCase();
                }
                if (ev.ctrlKey)
                        modifiers.push("C");
                if (ev.altKey)
                        modifiers.push("M");
                if (ev.shiftKey && ev.charCode && /^[a-zA-Z0-9]$/.test(key))
                        modifiers.push("S");
                modifiers.sort();
                modifiers = modifiers.join("-");
                if (modifiers)
                        modifiers += "-";
                return modifiers + key;
        };

        P.defineKey = function(key, func, args) {
                var cmd = func;
                if (func instanceof Array) {
                        func = func[0];
                        args = func.slice(1);
                }
                key = key.split(/\s*&&\s*/);
                if (key.length > 1) {
                        key.foreach(function(key){
                                this.defineKey(key, func, args);
                        }, this);
                } else {
                        if (typeof func == "string")
                                func = this.editor.COMMANDS[func];
                        key = key[0].trim();
                        var dfn = this.definitions;
                        if (key.indexOf(" ") >= 0) {
                                var a = key.split(/\s+/);
                                key = a.pop();
                                a.foreach(function(key){
                                        key = this.parseKey(key).str;
                                        if (!dfn[key])
                                                dfn[key] = {};
                                        dfn = dfn[key];
                                }, this);
                        }
                        key = this.parseKey(key);
                        dfn[key.str] = this.makeHandler(func, cmd, args);
                }
        };

        P.defineKeys = function(map) {
                Object.foreach(map, function(func, key){
                        this.defineKey(key, func);
                }, this);
        };

        P.makeHandler = function(func, cmd, args) {
                return this.editor.makeInteractiveHandler(func, cmd, args);
        };

        P.handleKeyEvent = function(ev) {
                var key = this.unparseKey(ev),
                    def = ( this.currentPrefix
                            ? this.currentPrefix[key]
                            : this.definitions[key] );
                this.currentKeys.push(key);
                if (def instanceof Function) {
                        this.currentPrefix = null;
                        this.currentKeys = [];
                        def();
                        return true;
                }
                if (this.currentPrefix && !def) {
                        this.currentPrefix = null;
                        this.editor.signalError(this.currentKeys.join(" ") + " is undefined");
                        this.currentKeys = [];
                        return true;
                }
                this.currentPrefix = def;
                if (!def)
                        def = this.defaultHandler();
                return def;
        };

});

DEFINE_CLASS("XCodeEditor_Keymap_Emacs", XCodeEditor_Keymap, function(D, P){

        D.KEYS = {
                // movement
                "ARROW_UP && C-p"            : "backward_line",
                "ARROW_DOWN && C-n"          : "forward_line",
                "ARROW_LEFT && C-b"          : "backward_char",
                "ARROW_RIGHT && C-f"         : "forward_char",
                "HOME"                       : "beginning_of_indentation_or_line",
                "END && C-e"                 : "end_of_line",
                "C-a"                        : "beginning_of_line",
                "C-HOME && M-<"              : "beginning_of_buffer",
                "C-END && M->"               : "end_of_buffer",
                "C-ARROW_RIGHT && M-f"       : "forward_word",
                "C-ARROW_LEFT && M-b"        : "backward_word",
                "C-ARROW_DOWN"               : "forward_paragraph",
                "C-ARROW_UP"                 : "backward_paragraph",
                "C-l"                        : "recenter_top_bottom",
                "PAGE_UP && M-v"             : "scroll_up",
                "PAGE_DOWN && C-v"           : "scroll_down",

                // basic editing
                "BACKSPACE"                  : "backward_delete_char",
                "DELETE && C-d"              : "delete_char",
                "ENTER && C-m"               : "newline",
                "M-d"                        : "kill_word",
                "C-BACKSPACE && M-BACKSPACE" : "backward_kill_word",
                "C-k"                        : "kill_line",
                "C-y"                        : "yank",
                "M-y"                        : "yank_pop",
                "C-SPACE"                    : "set_mark_command",
                "C-x C-x"                    : "exchange_point_and_mark",
                "C-w"                        : "kill_region",
                "M-t"                        : "transpose_words",
                "C-t"                        : "transpose_chars",
                "M-w"                        : "copy_region_as_kill",
                "M-c"                        : "capitalize_word",
                "M-u"                        : "upcase_word",
                "M-l"                        : "downcase_word",
                "F11"                        : "nuke_trailing_whitespace",
                "TAB"                        : "indent_line",
                "M-q"                        : "fill_paragraph",
                "C-/ && C-x u && C-_"        : "undo",
                "INSERT"                     : "overwrite_mode",
                "M-s"                        : "center_line",
                "C-s"                        : "isearch_forward",
                "C-r"                        : "isearch_backward",

                // necessary evil
                "C-S-y"                      : "yank_from_operating_system",
                "M-S-w"                      : "copy_for_operating_system",
                "C-S-w"                      : "kill_for_operating_system",

                // my stuff, sorry if these have different meaning from the standard Emacs keys
                "M-C-d"                      : "delete_region_or_line",
                "M-S-y"                      : "yank_shift", // that's the reverse of yank_shift

                // DEBUG
                "C-x =": function() {
                        alert(this.point());
                }
        };

        D.CONSTRUCT = function() {
                this.defineKeys(D.KEYS);
        };

});

DEFINE_CLASS("XCodeEditor_Keymap_ISearch", XCodeEditor_Keymap, function(D, P){

        D.KEYS = {
                "C-g && ESCAPE": [ "isearch_abort", true ],
                "C-w": "isearch_yank_word_or_char",
                "C-s": "isearch_forward",
                "C-r": "isearch_backward",
                "BACKSPACE": function() {
                        if (this.minibuffer.point() > this._isearchContext.mbMark.getPosition()) {
                                this.minibuffer.cmd("backward_delete_char");
                                this.cmd("goto_char", this._isearchContext.point);
                                updateIsearch.call(this, this._isearchContext.forward);
                        }
                },
                "ENTER": "isearch_abort"
        };

        D.CONSTRUCT = function() {
                this.defaultHandler = this.makeHandler(this.editor.COMMANDS["isearch_printing_char"], "isearch_printing_char");
                this.defineKeys(D.KEYS);
        };

        function initIsearch(fw) {
                if (!this._isearchContext) {
                        this.pushKeymap(this._keymap_isearch);
                        this.cmd("set_mark_command");
                        this.setMinibuffer(fw ? "I-Search: " : "I-Search backward: ");
                        this._isearchContext = {
                                forward : fw,
                                point   : this.point(),
                                mbMark  : this.minibuffer.createMarker(this.minibuffer.point(), true)
                        };
                        return true;
                }
        };

        function updateIsearch(fw) {
                this._isearchContext.forward = fw;
                this._isearchContext.point = this.point();
                var text = getText(this);
                if (!/\S/.test(text) && this._isearchLastText) {
                        this.minibuffer._placeUndoBoundary();
                        this.minibuffer.cmd("insert", this._isearchLastText);
                        text = this._isearchLastText;
                }
                return doSearch.call(this, text);
        };

        function doSearch(text) {
                if (text == null)
                        text = getText(this);
                var found = this.cmd("bind_variables", { case_fold_search: text == text.toLowerCase() },
                                     this.cmd,
                                     this._isearchContext.forward ? "search_forward" : "search_backward",
                                     text);
                if (found)
                        this.cmd("recenter_top_bottom");
                return found;
        };

        function getText(o) {
                return o.cmd("isearch_get_search_text");
        };

        XCodeEditor.newCommands({

                isearch_get_search_text: function() {
                        if (this._isearchContext) {
                                return this.minibuffer._bufferSubstring(this._isearchContext.mbMark);
                        }
                },

                isearch_forward: function() {
                        if (!initIsearch.call(this, true)) {
                                if (!updateIsearch.call(this, true))
                                        this.signalError("No more forward occurrences of the search text");
                        }
                },

                isearch_backward: function() {
                        if (!initIsearch.call(this, false)) {
                                if (!updateIsearch.call(this, false))
                                        this.signalError("No more backward occurrences of the search text");
                        }
                },

                isearch_yank_word_or_char: function() {
                        var pos = this.point();
                        this.cmd("forward_word");
                        var pos2 = this.point();
                        if (pos2 != pos) {
                                var word = this._bufferSubstring(pos, pos2);
                                this.minibuffer._placeUndoBoundary();
                                this.minibuffer.cmd("insert", word.toLowerCase());
                        }
                },

                isearch_printing_char: function() {
                        var ev = this.interactiveEvent;
                        if (ev.charCode && !ev.ctrlKey && !ev.altKey) {
                                this.minibuffer.cmd("self_insert_command", ev);
                                var text = getText(this);
                                this.cmd("goto_char", this._isearchContext.point);
                                doSearch.call(this, text);
                                return ev.domStop = true;
                        } else if (ev.keyCode != 0 || ev.ctrlKey || ev.altKey) {
                                this.cmd("isearch_abort");
                                return false;
                        }
                },

                isearch_abort: function(cancelled) {
                        if (!cancelled)
                                this._isearchLastText = getText(this);
                        this.setMinibuffer("");
                        this.popKeymap(this._keymap_isearch);
                        this._isearchContext.mbMark.destroy();
                        this._isearchContext = null;
                        if (cancelled)
                                this.cmd("exchange_point_and_mark");
                        return true;
                }

        });

});
