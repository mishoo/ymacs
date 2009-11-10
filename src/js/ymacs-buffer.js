// This file is part of Ymacs, an extensible source code editor
// (c) Mihai Bazon 2009 <mihai.bazon@gmail.com>
// Distributed under a BSD-style license.
// http://www.ymacs.org/

DEFINE_CLASS("Ymacs_Buffer", DlEventProxy, function(D, P){

        D.DEFAULT_EVENTS = [
                "onLineChange",
                "onInsertLine",
                "onDeleteLine",
                "onPointChange",
                "onResetCode",
                "onMessage",
                "onOverwriteMode",
                "onOverlayChange",
                "onOverlayDelete",
                "beforeInteractiveCommand",
                "afterInteractiveCommand"
        ];

        D.DEFAULT_ARGS = {
                name      : [ "name"      , "*scratch*" ],
                _code     : [ "code"      , null ],
                ymacs     : [ "ymacs"     , null ],
                tokenizer : [ "tokenizer" , null ]
        };

        var MAX_UNDO_RECORDS = 50000; // XXX: should we not limit?

        function MRK(x) {
                return x instanceof Ymacs_Marker ? x.getPosition() : x;
        };

        function TEST_UNICODE_WORD_CHAR(c) {
                if (c) {
                        var code = c.charCodeAt(0);
                        return (code >= 48 && code <= 57) || c.toUpperCase() != c.toLowerCase();
                }
        };

        function TEST_DABBREV_WORD_CHAR(c) {
                if (c) {
                        var code = c.charCodeAt(0);
                        return (code >= 48 && code <= 57) || c == "_" || c.toUpperCase() != c.toLowerCase();
                }
        };

        P.lastIndexOfRegexp = function(str, re, caret, bound) {
                str = str.substring(0, caret);
                re = Ymacs_Regexp.search_backward(re);
                re.lastIndex = bound || 0;
                var m = re.exec(str);
                if (m) {
                        var a = Array.$(m, 2);
                        a.index = m.index + m[1].length;
                        a.after = m.index + m[0].length;
                        a[0] = str.substring(a.index, a.after);
                        this.matchData = a;
                        return a;
                }
        };

        D.COMMANDS = {};

        D.newCommands = P.newCommands = function(cmds) {
                Object.merge(this.COMMANDS, cmds);
        };

        D.newMode = P.newMode = function(name, activate) {
                var modevar = "*" + name + "*";
                this.COMMANDS[name] = function(force) {
                        var on = !this.getq(modevar);
                        if (!on && force)
                                return true;
                        this.setq(modevar, on);
                        if (this.__deactivateCurrentMode) {
                                this.__deactivateCurrentMode();
                                this.__deactivateCurrentMode = null;
                        }
                        if (on) {
                                var off = activate.apply(this, arguments);
                                this.__deactivateCurrentMode = function() {
                                        off.call(this);
                                        this.setq(modevar, false);
                                };
                        }
                        return true;
                };
        };

        D.FIXARGS = function(args) {
                if (args.code == null)
                        args.code = "";
        };

        D.CONSTRUCT = function() {
                this.COMMANDS = Object.makeCopy(D.COMMANDS);

                this.__savingExcursion = 0;
                this.__preventUpdates = 0;
                this.__preventUndo = 0;
                this.__undoInProgress = 0;
                this.__inInteractiveCommand = 0;
                this.__dirtyLines = [];
                this.__undoQueue = [];
                this.__redoQueue = [];
                this.__overlays = {};

                this.markers = [];
                this.caretMarker = this.createMarker(0, false, "point");
                this.markMarker = this.createMarker(0, true, "mark");
                this.matchData = [];
                this.previousCommand = null;
                this.currentCommand = null;

                this.variables = {
                        case_fold_search            : true,
                        line_movement_requested_col : 0,
                        fill_column                 : 78,
                        tab_width                   : 8,
                        indent_level                : 8,

                        // syntax variables
                        syntax_word                 : { test: TEST_UNICODE_WORD_CHAR },
                        syntax_word_dabbrev         : { test: TEST_DABBREV_WORD_CHAR },
                        syntax_paragraph_sep        : /\n\s*\n/g
                };

                this.caretMarker.onChange.push(function(pos) {
                        this._rowcol = this.caretMarker.getRowCol();
                        // XXX: this shouldn't be needed
                        if (this.__inInteractiveCommand == 0 && this.__preventUpdates == 0) {
                                this.callHooks("onPointChange", this._rowcol, this.point());
                        }
                });

                this._tokenizerEvents = {
                        "onFoundToken": this._on_tokenizerFoundToken.$(this)
                };

                this._textProperties = new Ymacs_Text_Properties({});
                this._textProperties.addEventListener("onChange", this._on_textPropertiesChange.$(this));

                this.keymap = [];
                this._keymap_isearch = new Ymacs_Keymap_ISearch({ buffer: this });
                this.pushKeymap(this.makeDefaultKeymap());
                this.setCode(this._code);
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
                        if (cont instanceof Function)
                                return cont.apply(this, Array.$(arguments, 2));
                        else
                                return this.cmdApply(cont, Array.$(arguments, 2));
                } finally {
                        for (i in saved)
                                this.variables[i] = saved[i];
                }
        };

        P.getVariable = function(key) {
                return this.variables[key];
        };

        P.setVariable = function(key, val) {
                if (typeof key == "string") {
                        return this.variables[key] = val;
                } else {
                        var changed = {};
                        for (var i in key) {
                                changed[i] = this.variables[i];
                                if (key[i] === undefined)
                                        delete this.variables[i];
                                else
                                        this.variables[i] = key[i];
                        }
                        return changed;
                }
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
                return new Ymacs_Keymap_Emacs({ buffer: this });
        };

        P.signalError = function(text, html) {
                this.callHooks("onMessage", "error", text, html);
        };

        P.signalInfo = function(text, html) {
                this.callHooks("onMessage", "info", text, html);
        };

        P.createMarker = function(pos, before, name) {
                if (pos == null)
                        pos = this.point();
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
                this.__overlays = {};
                this.markers = [ this.caretMarker, this.markMarker ]; // resetting the code invalidates markers
                this.code = code.split(/\n/);
                this._textProperties.reset();
                if (this.tokenizer) {
                        this.tokenizer.reset();
                }
                this.callHooks("onResetCode", this.code);
                this.caretMarker.setPosition(0, false, true);
                this.markMarker.setPosition(0, true);
        };

        P.setTokenizer = function(tok) {
                if (this.tokenizer != null) {
                        this.tokenizer.removeEventListener(this._tokenizerEvents);
                }
                this.tokenizer = tok;
                if (tok) {
                        tok.addEventListener(this._tokenizerEvents);
                } else {
                        this._textProperties.reset();
                        this.callHooks("onResetCode", this.code);
                }
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
                var n = this.code.length;
                if (row >= n--)
                        return null;
                var line = this.code[row];
                if (col == line.length)
                        return row == n && line.charAt(col) || "\n";
                return line.charAt(col);
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
                        ++this.__inInteractiveCommand;
                        this.preventUpdates();
                        try {
                                this.callHooks("beforeInteractiveCommand");
                                return func.apply(this, arguments);
                        } finally {
                                if (!/_mark$/.test(this.currentCommand))
                                        this.clearTransientMark();
                                this.resumeUpdates();
                                --this.__inInteractiveCommand;
                                this.callHooks("afterInteractiveCommand");
                                this.previousCommand = cmd;
                                this.sameCommandCount++;
                                this.ensureCaretVisible();
                        }
                }.$A(this, args);
        };

        P.resetOverwriteMode = function(om) {
                if (arguments.length == 0)
                        om = this.overwriteMode;
                this.callHooks("onOverwriteMode", this.overwriteMode = !om);
                this.signalInfo(om ? "Insert mode" : "Overwrite mode");
        };

        P.getMinibuffer = function() {
                return this.whenYmacs(function(ymacs) { return ymacs.minibuffer; });
        };

        P.setMinibuffer = function(text) {
                this.whenMinibuffer(function(mb){
                        mb.setCode(text);
                        mb.cmd("end_of_buffer");
                });
        };

        P.ensureCaretVisible = function() {
                this.whenActiveFrame("ensureCaretVisible");
        };

        P.cmd = function(cmd) {
                return this.COMMANDS[cmd].apply(this, Array.$(arguments, 1));
        };

        P.cmdApply = function(cmd, args) {
                return this.COMMANDS[cmd].apply(this, args);
        };

        P.cmdRepeat = function(times) {
                var args = Array.$(arguments, 1);
                while (times-- > 0)
                        this.cmd.apply(this, args);
        };

        P.createDialog = function(args) {
                if (!args.parent)
                        args.parent = this.getActiveFrame() && this.getActiveFrame().getParentDialog();
                var dlg = new DlDialog(args);
                this.whenActiveFrame(function(frame){
                        dlg.addEventListener("onDestroy", frame.focus.clearingTimeout(0, frame));
                });
                return dlg;
        };

        P.getActiveFrame = function() {
                return this.whenYmacs("getActiveFrame");
        };

        // This function receives a string and a continuation.  If
        // there is an object property or variable named $what, then
        // $cont is called in the context of this object and given the
        // value of $what as first argument.  The returned value is
        // passed back to caller.
        //
        // The continuation can also be a string, in which case it's
        // assumed to be a method in the value of $what, thus called
        // on it.
        //
        // This is a bit messy, but should work well as long as we
        // don't use the same name for both an object property and a
        // variable in this.variables.  Otherwise, the property takes
        // precedence.
        P.when = function(what, cont) {
                what = this[what] || this.getq(what);
                if (what != null) {
                        if (cont instanceof Function)
                                return cont.call(this, what);
                        else {
                                return what[cont].apply(what, Array.$(arguments, 2));
                        }
                }
        };

        // XXX: this is way too ugly.
        P.whenActiveFrame = function() {
                var fr = this.getActiveFrame(); // miserable hack
                if (fr.buffer === this) {
                        this.activeFrame = fr;
                        var a = Array.$(arguments);
                        a.unshift("activeFrame");
                        return this.when.apply(this, a);
                } else {
                        this.activeFrame = null;
                }
        };

        P.whenYmacs = function() {
                var a = Array.$(arguments);
                a.unshift("ymacs");
                return this.when.apply(this, a);
        };

        P.whenMinibuffer = function(cont) {
                // In fact, we should move when() into some base
                // object... but which one?  JS doesn't have multiple
                // inheritance, though we could easily "invent" it.
                return this.whenYmacs(function(ymacs){
                        if (ymacs.minibuffer)
                                return cont.call(this, ymacs.minibuffer);
                });
        };

        P.preventUpdates = function() {
                ++this.__preventUpdates;
        };

        P.resumeUpdates = function() {
                if ((this.__preventUpdates = Math.max(this.__preventUpdates - 1, 0)) == 0) {
                        this.redrawDirtyLines();
                }
        };

        P.redrawDirtyLines = function() {
                this.__dirtyLines.foreach(function(draw, row){
                        if (draw)
                                this.callHooks("onLineChange", row);
                }, this);
                this.__dirtyLines = [];
        };

        P.getOverlays = function() {
                return this.__overlays;
        };

        P.getOverlay = function(name) {
                return this.__overlays[name];
        };

        P.setOverlay = function(name, props) {
                var ov = this.__overlays[name], isNew = !ov, tmp;
                if (isNew)
                        ov = this.__overlays[name] = props;
                else
                        Object.merge(ov, props);
                // normalize line/col
                if (ov.line2 < ov.line1) {
                        tmp = ov.line2; ov.line2 = ov.line1; ov.line1 = tmp;
                        tmp = ov.col2; ov.col2 = ov.col1; ov.col1 = tmp;
                }
                else if (ov.line2 == ov.line1 && ov.col2 < ov.col1) {
                        tmp = ov.col2; ov.col2 = ov.col1; ov.col1 = tmp;
                }
                this.callHooks("onOverlayChange", name, ov, isNew);
        };

        P.deleteOverlay = function(name) {
                delete this.__overlays[name];
                this.callHooks("onOverlayDelete", name);
        };

        P.ensureTransientMark = function() {
                var rc = this._rowcol, tm;
                if (!this.transientMarker) {
                        this.transientMarker = this.createMarker(this.point());
                        this.markMarker.setPosition(this.point());
                        tm = rc;
                }
                if (!tm)
                        tm = this._positionToRowCol(this.transientMarker.getPosition());
                this.setOverlay("selection", {
                        line1 : tm.row,
                        col1  : tm.col,
                        line2 : rc.row,
                        col2  : rc.col
                });
        };

        P.clearTransientMark = function() {
                if (this.transientMarker) {
                        this.transientMarker.destroy();
                        this.transientMarker = null;
                        this.deleteOverlay("selection");
                }
        };

        P.deleteTransientRegion = function() {
                if (this.transientMarker) {
                        this._deleteText(this.caretMarker, this.transientMarker);
                        this.clearTransientMark();
                        return true;
                }
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
                // this._textProperties[row] = null; // XXX: OLD
                this.code[row] = text;
                if (this.__preventUpdates == 0) {
                        this.callHooks("onLineChange", row);
                } else {
                        this.__dirtyLines[row] = true;
                }
        };

        P._deleteLine = function(row) {
                this.code.splice(row, 1);
                this._textProperties.deleteLine(row);
                if (this.tokenizer)
                        this.tokenizer.quickDeleteLine(row);
                if (this.__preventUpdates != 0)
                        this.__dirtyLines.splice(row, 1);
                this.callHooks("onDeleteLine", row);
        };

        P._insertLine = function(row, text) {
                this.code.splice(row, 0, text);
                this._textProperties.insertLine(row);
                if (this.tokenizer)
                        this.tokenizer.quickInsertLine(row);
                if (this.__preventUpdates == 0) {
                        this.callHooks("onInsertLine", row, true);
                } else {
                        this.__dirtyLines.splice(row, 0, true);
                        this.callHooks("onInsertLine", row);
                }
        };

        P._insertText = function(text, pos) {
                if (pos == null)
                        pos = this.caretMarker.getPosition();
                pos = MRK(pos);
                // *** UNDO RECORDING
                if (this.__preventUndo == 0)
                        this._recordChange(1, pos, text.length);
                var rc = pos == this.point() ? this._rowcol : this._positionToRowCol(pos);
                var lines = text.split(/\n/), i = rc.row, rest = this.code[i].substr(rc.col);
                if (lines.length > 1) {
                        this._replaceLine(i, this.code[i].substr(0, rc.col) + lines.shift());
                        lines.foreach(function(text){
                                this._insertLine(++i, text);
                        }, this);
                        this._replaceLine(i, this.code[i] + rest);
                } else {
                        this._replaceLine(i, this.code[i].substr(0, rc.col) + lines[0] + this.code[i].substr(rc.col));
                }
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
                        this.ymacs.killRingToMaster();
                }
                this.ymacs.pushToKillRing(text, prepend);
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
                var pos = 0, i = Math.min(row, this.code.length - 1), n = i;
                if (i < 0)
                        return 0;
                while (--i >= 0)
                        pos += this.code[i].length + 1; // one for the newline
                return pos + Math.min(col, this.code[n].length);
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
                if (this.__undoInProgress == 0) {
                        this.markers.map("editorChange", offset, delta, min || 0);
                }
                if (this.tokenizer) {
                        this.tokenizer.quickUpdate(Math.min(offset, offset + delta));
                }
        };

        P._saveExcursion = function(cont) {
                var tmp = this.createMarker(this.point());
                ++this.__savingExcursion;
                var ret;
                try {
                        return cont.call(this, tmp.getPosition());
                } finally {
                        --this.__savingExcursion;
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
                this.whenActiveFrame("centerOnCaret");
        };

        P._on_tokenizerFoundToken = function(row, c1, c2, what) {
                if (what) {
                        this._textProperties.addLineProps(row, c1, c2, "css", what);
                } else {
                        this._textProperties.removeLineProps(row, c1, c2, "css");
                }
        };

        P._on_textPropertiesChange = function(row) {
                this.lastColoredLine = row;
                if (this.__preventUpdates == 0) {
                        this.callHooks("onLineChange", row);
                } else {
                        this.__dirtyLines[row] = true;
                }
        };

        P.formatLineHTML = function(row) {
                return this._textProperties.getLineHTML(row, this.code[row]);
        };

});
