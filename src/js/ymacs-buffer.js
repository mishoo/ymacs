DEFINE_CLASS("Ymacs_Buffer", DlEventProxy, function(D, P){

        D.DEFAULT_EVENTS = [
                "onLineChange",
                "onInsertLine",
                "onDeleteLine",
                "onPointChange",
                "onResetCode",
                "onMessage",
                "onOverwriteMode"
        ];

        var MAX_UNDO_RECORDS = 50000; // XXX: should we not limit?
        var REQUEUE_REDO = 0;

        function MRK(x) {
                return x instanceof Ymacs_Marker ? x.getPosition() : x;
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

        D.COMMANDS = {};

        D.newCommands = P.newCommands = function(cmds) {
                Object.merge(this.COMMANDS, cmds);
        };

        D.DEFAULT_ARGS = {
                _code: [ "code", null ]
        };

        D.FIXARGS = function(args) {
                if (args.code == null)
                        args.code = "";
        };

        D.CONSTRUCT = function() {
                this.COMMANDS = Object.makeCopy(D.COMMANDS);
                this.markers = [];
                this.__savingExcursion = 0;
                this.__preventUpdates = 0;
                this.__preventUndo = 0;
                this.__undoQueue = [];
                this.__redoQueue = [];
                this.__undoInProgress = 0;
                this.caretMarker = this.createMarker(0, false, "point");
                this.markMarker = this.createMarker(0, true, "mark");
                this.matchData = [];
                this.previousCommand = null;
                this.currentCommand = null;
                this.inInteractiveCommand = 0;
                this.variables = {
                        case_fold_search : true,
                        fill_column      : 78,
                        tab_width        : 8
                };
                this.caretMarker.onChange.push(function(pos) {
                        this._rowcol = this.caretMarker.getRowCol();
                        // XXX: this shouldn't be needed
                        if (this.inInteractiveCommand == 0 && this.__savingExcursion == 0)
                                this.callHooks("onPointChange", this._rowcol, this.point());
                });
                this.syntax = {
                        word_ng       : { test: TEST_UNICODE_WORD_CHAR },
                        paragraph_sep : /\n\s*\n/g
                };
                this.keymap = [];
                this._keymap_isearch = new Ymacs_Keymap_ISearch({ editor: this });
                this.pushKeymap(this.makeDefaultKeymap());
                this.setCode(this._code);
                this.killRing = [];
                this.killMasterOfRings = [];
                this._lastCommandWasKill = 0;
                delete this["_code"];
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

        P.pushKeymap = function(keymap) {
                this.popKeymap(keymap);
                this.keymap.push(keymap);
        };

        P.popKeymap = function(keymap) {
                this.keymap.remove(keymap);
        };

        P.makeDefaultKeymap = function() {
                return new Ymacs_Keymap_Emacs({ editor: this });
        };

        P.signalError = function(text) {
                this.callHooks("onMessage", "error", text);
        };

        P.signalInfo = function(text) {
                this.callHooks("onMessage", "info", text);
        };

        P.createMarker = function(pos, before, name) {
                return new Ymacs_Marker({ editor: this, pos: pos, name: name, before: before });
        };

        P.point = function() {
                return this.caretMarker.getPosition();
        };

        P.setCode = function(code) {
                this.__code = code = code.replace(/\t/g, " ".x(this.getq("tab_width")));
                this.__size = code.length;
                this.__undoQueue = [];
                this.__redoQueue = [];
                this.markers = [ this.caretMarker, this.markMarker ]; // resetting the code invalidates markers
                this.code = code.split(/\n/);
                this.callHooks("onResetCode", this.code);
                this.caretMarker.setPosition(0, false, true);
                this.markMarker.setPosition(0, true);
        };

        P.getCode = function() {
                return this.__code || (this.__code = this.code.join("\n"));
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
                        point = MRK(point);
                        if (point < 0)
                                point += this.point();
                }
                var rc = this._positionToRowCol(point);
                return this.charAtRowCol(rc.row, rc.col);
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
                this.callHooks("onOverwriteMode", this.overwriteMode = !om);
        };

        P.setMinibuffer = function(text) {
                if (this.minibuffer) {
                        this.minibuffer.setCode(text);
                        this.minibuffer.cmd("end_of_buffer");
                }
        };

        P.ensureCaretVisible = function() {
                if (this.activeFrame)
                        this.activeFrame.ensureCaretVisible();
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

        P.createDialog = function(args) {
                if (!args.parent)
                        args.parent = this.activeFrame && this.activeFrame.getParentDialog();
                var dlg = new DlDialog(args);
                this.whenActiveFrame(function(frame){
                        dlg.addEventListener("onDestroy", frame.focus.clearingTimeout(0, frame));
                });
                return dlg;
        };

        P.focus = function() {
                if (this.activeFrame)
                        this.activeFrame.focus();
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
                if (this.__preventUpdates == 0)
                        this.callHooks("onLineChange", row, text);
        };

        P._deleteLine = function(row) {
                this.code.splice(row, 1);
                if (this.__preventUpdates == 0)
                        this.callHooks("onDeleteLine", row);
        };

        P._insertLine = function(row, text) {
                this.code.splice(row, 0, text);
                if (this.__preventUpdates == 0)
                        this.callHooks("onInsertLine", row, text);
        };

        P._insertText = function(text, pos) {
                if (pos == null)
                        pos = this.caretMarker.getPosition();
                pos = MRK(pos);
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
                begin = MRK(begin);
                end = MRK(end);
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
                a = a.map(MRK).mergeSort();
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
                else begin = MRK(begin);

                if (end == null) end = this.getCodeSize();
                else end = MRK(end);

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
                p1 = MRK(p1);
                p2 = MRK(p2);
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

        P._repositionCaret = function(pos) {
                var p = this.caretMarker.getPosition();
                if (pos == null)
                        pos = p;
                pos = MRK(pos);
                pos = this._boundPosition(pos);
                this.caretMarker.setPosition(pos);
                return pos != p;
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
                                        this.callHooks("onResetCode");
                        this.caretMarker.swap(tmp, false, true);
                        tmp.destroy();
                }
        };

        P._handleKeyEvent = function(ev) {
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
                return handled;
        };

        P._centerOnCaret = function() {
                this.activeFrame.centerOnCaret();
        };

        P.getActiveFrame = function() {
                return this.activeFrame;
        };

        P.whenActiveFrame = function(cont) {
                var frame = this.getActiveFrame();
                if (frame)
                        return cont.call(this, frame);
        };

});
