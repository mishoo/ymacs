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

// @require ymacs-interactive.js

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
        "afterInteractiveCommand",
        "beforeRedraw",
        "afterRedraw",
        "finishedEvent",
        "onProgressChange",
        "onTextInsert",
        "onTextDelete"
    ];

    D.DEFAULT_ARGS = {
        name         : [ "name"         , "*scratch*" ],
        _code        : [ "code"         , null ],
        ymacs        : [ "ymacs"        , null ],
        tokenizer    : [ "tokenizer"    , null ],
        isMinibuffer : [ "isMinibuffer" , false ]
    };

    var GLOBAL_VARS = {
        case_fold_search            : true,
        line_movement_requested_col : 0,
        fill_column                 : 78,
        tab_width                   : 8,
        indent_level                : 4,

        // syntax variables
        syntax_word                 : { test: TEST_UNICODE_WORD_CHAR },
        syntax_word_dabbrev         : { test: TEST_DABBREV_WORD_CHAR },
        syntax_paragraph_sep        : /\n\s*\n/g
    };

    function setq(key, val) {
        if (typeof key == "string") {
            if (val === undefined)
                delete this[key];
            else
                this[key] = val;
            if (val instanceof Function)
                val.ymacsCommand = key;
            return val;
        } else {
            var changed = {};
            for (var i in key) {
                changed[i] = this[i];
                setq.call(this, i, key[i]);
            }
            return changed;
        }
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

    D.COMMANDS = P.COMMANDS = {};

    D.newCommands = P.newCommands = function() {
        return setq.apply(this.COMMANDS, arguments);
    };

    D.replaceCommands = P.replaceCommands = function(cmds) {
        this.COMMANDS = Object.makeCopy(this.COMMANDS);
        var replacements = {};
        Object.foreach(cmds, function(newcmd, oldcmd){
            if (typeof newcmd == "string") newcmd = this[newcmd];
            replacements[oldcmd] = newcmd;
        }, this.COMMANDS);
        return this.newCommands(replacements);
    };

    D.newMode = P.newMode = function(name, activate) {
        var modevar = "*" + name + "*", hookvar = modevar + "hooks";
        D.setGlobal(hookvar, []);
        this.COMMANDS[name] = Ymacs_Interactive("P", function(force){
            var status = this.getq(modevar);
            if (status) {
                // currently active
                if (force !== true) {
                    // deactivate
                    this.getq(hookvar).foreach(function(hook){
                        hook.call(this, false);
                    }, this);
                    if (status instanceof Function) {
                        // clean-up
                        status.call(this);
                    }
                    this.setq(modevar, null);
                    this.modes.remove(name);
                }
            }
            else {
                // inactive
                if (force !== false) {
                    var off = activate.apply(this, arguments);
                    if (!(off instanceof Function))
                        off = true;
                    this.setq(modevar, off);
                    this.modes.push(name);
                    this.getq(hookvar).foreach(function(hook){
                        hook.call(this, true);
                    }, this);
                }
            }
            return status;
        });
    };

    D.addModeHook = P.addModeHook = function(name, func) {
        if (typeof func == "string")
            func = this.COMMANDS[func];
        var hookvar = "*" + name + "*hooks";
        this.getq(hookvar).pushUnique(func);
    };

    D.removeModeHook = P.removeModeHook = function(name, func) {
        if (typeof func == "string")
            func = this.COMMANDS[func];
        var hookvar = "*" + name + "*hooks";
        this.getq(hookvar).remove(func);
    };

    D.FIXARGS = function(args) {
        if (args.code == null)
            args.code = "";
    };

    D.CONSTRUCT = function() {
        this.__savingExcursion = 0;
        this.__preventUpdates = 0;
        this.__preventUndo = 0;
        this.__undoInProgress = 0;
        this.__dirtyLines = [];
        this.__undoQueue = [];
        this.__undoPointer = 0;
        this.__overlays = {};

        this.markers = [];
        this.caretMarker = this.createMarker(0, false, "point");
        this.markMarker = this.createMarker(0, true, "mark");
        this.matchData = [];
        this.previousCommand = null;
        this.currentCommand = null;
        this.currentKeys = [];
        this.progress = {};

        this.variables = {};
        this.globalVariables = GLOBAL_VARS;
        this.modes = [];

        this.caretMarker.onChange.push(function(pos) {
            this._rowcol = this.caretMarker.getRowCol();
            // XXX: this shouldn't be needed
            if (this.__preventUpdates == 0) {
                this.callHooks("onPointChange", this._rowcol, this.point());
            }
        });

        this._tokenizerEvents = {
            "onFoundToken": this._on_tokenizerFoundToken.$(this)
        };

        this._textProperties = new Ymacs_Text_Properties({ buffer: this });
        this._textProperties.addEventListener("onChange", this._on_textPropertiesChange.$(this));

        this.keymap = [];
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
            for (i in saved) {
                if (saved[i] === undefined)
                    delete this.variables[i];
                else
                    this.variables[i] = saved[i];
            }
        }
    };

    P.withCommands = function(cmds, cont) {
        var saved = this.COMMANDS;
        this.COMMANDS = Object.makeCopy(saved);
        Object.merge(this.COMMANDS, cmds);
        try {
            if (cont instanceof Function)
                return cont.apply(this, Array.$(arguments, 2));
            else
                return this.cmdApply(cont, Array.$(arguments, 2));
        } finally {
            this.COMMANDS = saved;
        }
    };

    P.getVariable = function(key) {
        return (key in this.variables)
            ? this.variables[key]
            : GLOBAL_VARS[key];
    };

    P.setVariable = function() {
        return setq.apply(this.variables, arguments);
    };

    D.setq = D.setVariable = D.setGlobal = P.setGlobal = function() {
        return setq.apply(GLOBAL_VARS, arguments);
    };

    P.setq = P.setVariable;
    P.getq = P.getVariable;
    D.getq = D.getVariable = function(key) {
        return GLOBAL_VARS[key];
    };

    /* -----[ public API ]----- */

    P.pushKeymap = function(keymap) {
        if (keymap instanceof Array) {
            keymap.foreach(this.pushKeymap, this);
        } else {
            this.popKeymap(keymap);
            this.keymap.push(keymap);
            keymap.attached(this);
        }
    };

    P.popKeymap = function(keymap) {
        this.keymap.remove(keymap);
        keymap.detached(this);
    };

    P.makeDefaultKeymap = function() {
        return Ymacs_Keymap_Emacs();
    };

    P.signalError = function(text, html, timeout) {
        // Terminates a running macro
        this.ymacs.indicateError();
        this.callHooks("onMessage", "error", text, html, timeout);
    };

    P.signalInfo = function(text, html, timeout) {
        this.callHooks("onMessage", "info", text, html, timeout);
    };

    P.createMarker = function(pos, before, name) {
        if (pos == null)
            pos = this.point();
        return new Ymacs_Marker({ editor: this, pos: pos, name: name, before: before });
    };

    P.point = function() {
        return this.caretMarker.getPosition();
    };

    P.dirty = function(dirty) {
        if (arguments.length > 0) {
            this.__isDirty = dirty;
            this.__undoQueue.foreach(function(x){
                if (x.type !== 3) x.dirty = true;
            });
            this.updateModeline();
        }
        return this.__isDirty;
    };

    P.setCode = function(code) {
        // this.__code = code = code.replace(/\t/g, " ".x(this.getq("tab_width")));
        this.__isDirty = false;
        this.__code = code;
        this.__size = code.length;
        this.__undoQueue = [];
        this.__undoPointer = 0;
        this.__overlays = {};
        this.markers.map("setPosition", 0, true, true);
        this.code = code.split(/\n/);
        this._textProperties.reset();
        if (this.tokenizer) {
            this.tokenizer.reset();
        }
        this.callHooks("onResetCode", this.code);
        this.caretMarker.setPosition(0, false, true);
        this.markMarker.setPosition(0, true);
        this.forAllFrames(function(frame){
            frame.ensureCaretVisible();
            frame.redrawModelineWithTimer();
        });
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
        var i = this.code.length, size = i > 0 ? -1 : 0;
        while (--i >= 0)
            size += this.code[i].length + 1;
        return this.__size = size;
    };

    P.getLine = function(row) {
        if (row == null)
            row = this._rowcol.row;
        return this.code[row];
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

    P.callInteractively = function(func, args, finalArgs) {
        if (!args)
            args = []; // make IE happy
        var cmd;
        if (!(func instanceof Function)) {
            cmd = func;
            func = this.COMMANDS[func];
        } else {
            cmd = func.ymacsCommand || null;
        }
        if (func.ymacsCallInteractively && !finalArgs) {
            // after prompting for eventual
            // arguments, ymacsCallInteractively
            // will actually call back again
            // buffer's callInteractively, so we
            // should STOP here.
            return func.ymacsCallInteractively.apply(this, args);
        }
        this.currentCommand = cmd;
        if (this.previousCommand != cmd) {
            this.sameCommandCount(0);
            if (cmd != "undo") {
                this._placeUndoBoundary();
            }
        } else if (cmd != "self_insert_command" || this.sameCommandCount() % 20 == 0) {
            if (cmd != "undo") {
                this._placeUndoBoundary();
            }
        }
        this.preventUpdates();
        try {
            this.callHooks("beforeInteractiveCommand", cmd, func);
            if (!func.ymacsMarkExtend)
                this.clearTransientMark();
            return func.apply(this, args);
        } catch(ex) {
            if (ex instanceof Ymacs_Exception) {
                this.signalError(ex.message);
            } else {
                throw ex;
            }
        } finally {
            if (cmd != "undo") {
                this.__undoPointer = this.__undoQueue.length;
            }
            this.resumeUpdates();
            this.callHooks("afterInteractiveCommand", cmd, func);
            this.previousCommand = cmd;
            this.sameCommandCount(+1);
        }
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

    P.getMinibufferFrame = function() {
        return this.whenYmacs(function(ymacs) { return ymacs.minibuffer_frame; });
    };

    P.setMinibuffer = function(text) {
        this.whenMinibuffer(function(mb){
            mb.setCode(text);
            mb.cmd("end_of_buffer");
        });
    };

    P.cmd = function(cmd) {
        return this.COMMANDS[cmd].apply(this, Array.$(arguments, 1));
    };

    P.cmdApply = function(cmd, args) {
        return this.COMMANDS[cmd].apply(this, args);
    };

    P.createDialog = function(args) {
        if (!args.parent) {
            args.parent = this.getActiveFrame() && this.getActiveFrame().getParentDialog();
            if (!("noShadows" in args)) {
                args.noShadows = true;
            }
        }
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

    P.forAllFrames = function(cont) {
        if (this.ymacs)
            this.ymacs.getBufferFrames(this).foreach(cont);
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

    P.getRegion = function(begin, end) {
        if (begin == null) begin = this.caretMarker;
        if (end == null) end = this.markMarker;
        begin = MRK(begin);
        end = MRK(end);
        if (end < begin) { var tmp = begin; begin = end; end = tmp; }
        return { begin: begin, end: end };
    };

    P.redrawDirtyLines = function() {
        this.callHooks("beforeRedraw");
        this.__dirtyLines.foreach(function(draw, row){
            if (draw) {
                this.callHooks("onLineChange", row);
            }
        }, this);
        this.__dirtyLines = [];
        this.callHooks("afterRedraw");
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
            this.transientMarker = this.createMarker();
            this.markMarker.setPosition(this.point());
            tm = rc;
        }
        if (!tm)
            tm = this.transientMarker.getRowCol();
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
            this._placeUndoBoundary();
            return true;
        }
    };

    var $sameCommandCount = 0;
    P.sameCommandCount = function(diff) {
        if (diff == null) return $sameCommandCount;
        if (diff == 0) return $sameCommandCount = 0;
        return $sameCommandCount += diff;
    };

    var $lastKeyEvent;
    P.interactiveEvent = function(ev) {
        if (arguments.length == 0)
            return $lastKeyEvent;
        return $lastKeyEvent = ev;
    };

    P.getPrefixArg = function(noDiscard) {
        var ret = this.getq("universal_prefix");
        if (!noDiscard) {
            this.setq("universal_prefix", undefined);
            if (!this.isMinibuffer)
                this.setMinibuffer("");
        }
        return ret;
    };

    P.setPrefixArg = function(val) {
        return this.setq("universal_prefix", val);
    };

    P.updateProgress = function(name, val) {
        if (val == null)
            delete this.progress[name];
        else
            this.progress[name] = val;
        this.callHooks("onProgressChange");
    };

    P.updateModeline = function() {
        this.callHooks("onProgressChange");
    };

    P.renderModelineContent = function(rc, percent) {
        var ml = String.buffer(
            this.__isDirty ? "**" : "--",
            " <b>",
            this.name.htmlEscape(),
            "</b>",
            "  ", percent, " of ", this.getCodeSize().formatBytes(2).toLowerCase(), "  ",
            "(", rc.row + 1, ",", rc.col, ") "
        );
        var custom = this.getq("modeline_custom_handler");
        if (custom) {
            custom = custom.call(this, this, rc);
            if (custom) ml("[", custom, "] ");
        }
        var pr = [];
        for (var i in this.progress) {
            pr.push(i + ": " + this.progress[i]);
        }
        if (pr.length > 0) {
            ml("{", pr.join(", "), "}");
        }
        return ml.get();
    };

    /* -----[ not-so-public API ]----- */

    // BEGIN: undo queue

    P._recordChange = function(type, pos, len, text) {
        if (len > 0) {
            var q = this.__undoQueue;
            q.push({
                type  : type,
                pos   : pos,
                len   : len,
                text  : text,
                dirty : this.__isDirty
            });
            this.__isDirty = true;
            if (q.length > MAX_UNDO_RECORDS)
                q.shift();
        }
    };

    P._placeUndoBoundary = function() {
        var q = this.__undoQueue;
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

    P._playbackUndo = function() {
        var q = this.__undoQueue;
        if (q.length == 0) return false;
        ++this.__undoInProgress;
        var didit = false, action;
        while (--this.__undoPointer >= 0) {
            action = q[this.__undoPointer];
            if (action.type == 3) { // boundary
                // restore markers
                action.markers.foreach(function(m){
                    m[0].setPosition(m[1]);
                });
                if (!didit) continue;
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
            this.__isDirty = action.dirty;
        }
        --this.__undoInProgress;
        return didit;
    };

    // END: undo

    P._replaceLine = function(row, text) {
        this.code[row] = text;
        this._textProperties.replaceLine(row, text);
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
        this.__dirtyLines.splice(row, 1);
        this.callHooks("onDeleteLine", row);
    };

    P._insertLine = function(row, text) {
        this.code.splice(row, 0, text);
        this._textProperties.insertLine(row);
        if (this.tokenizer)
            this.tokenizer.quickInsertLine(row);
        var drawIt = this.__preventUpdates == 0;
        this.callHooks("onInsertLine", row, drawIt);
        if (!drawIt) {
            if (this.__dirtyLines.length <= row)
                this.__dirtyLines[row] = true;
            else
                this.__dirtyLines.splice(row, 0, true);
        }
    };

    P._insertText = function(text, pos) {
        if (text.length == 0)
            return;
        if (pos == null)
            pos = this.caretMarker.getPosition();
        pos = MRK(pos);
        // *** UNDO RECORDING
        if (this.__preventUndo == 0)
            this._recordChange(1, pos, text.length);
        var rc = pos == this.point() ? this._rowcol : this._positionToRowCol(pos),
        i = rc.row;
        if (/^\n+$/.test(text) && rc.col == 0) {
            // handle this case separately, since it's so
            // frequently used (ENTER pressed) and the
            // default algorithm messes up colorization
            // for a fraction of a second, flashing badly.
            text.length.times(function(j){
                this._insertLine(i + j, "");
            }, this);
        } else {
            var lines = text.split("\n"), ln = this.code[i], rest = ln.substr(rc.col);
            if (lines.length > 1) {
                this._replaceLine(i, ln.substr(0, rc.col) + lines.shift());
                lines.foreach(function(text){
                    this._insertLine(++i, text);
                }, this);
                this._replaceLine(i, this.code[i] + rest);
            } else {
                this._replaceLine(i, ln.substr(0, rc.col) + lines[0] + ln.substr(rc.col));
            }
        }
        this._updateMarkers(pos, text.length);
        this.callHooks("onTextInsert", pos, text);
    };

    P._deleteText = function(begin, end) {
        begin = this._boundPosition(MRK(begin));
        end = this._boundPosition(MRK(end));
        if (begin == end)
            return;
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
        this.callHooks("onTextDelete", begin, end);
    };

    P._replaceText = function(begin, end, text) {
        this._deleteText(begin, end);
        this._insertText(text, begin);
    };

    P._swapAreas = function(a) {
        a = a.map(MRK).mergeSort();
        var b1 = a[0];
        var e1 = a[1];
        var b2 = a[2];
        var e2 = a[3];
        var t1 = this._bufferSubstring(b1, e1);
        var t2 = this._bufferSubstring(b2, e2);
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
        this._saveKilledText(text, prepend);
        if (!noDelete)
            this._deleteText(p1, p2);
    };

    P._saveKilledText = function(text, prepend) {
        if (!this._lastCommandWasKill)
            this.ymacs.killRingToMaster();
        this.ymacs.pushToKillRing(text, prepend);
        this._lastCommandWasKill++;
    };

    P._positionToRowCol = function(pos) {
        var line = 0, a = this.code, n = a.length;
        while (pos > 0 && line < n) {
            var len = a[line].length;
            if (len >= pos)
                break;
            pos -= len + 1; // one for the newline
            line++;
        }
        return { row: line, col: pos };
    };

    P._rowColToPosition = function(row, col) {
        var pos = 0, a = this.code, i = Math.min(row, a.length - 1), n = i;
        if (i < 0)
            return 0;
        while (--i >= 0)
            pos += a[i].length + 1; // one for the newline
        return pos + Math.min(col, a[n].length);
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
        // if (this.__undoInProgress == 0) {
        this.markers.map("editorChange", offset, delta, min || 0);
        // }
        if (this.tokenizer) {
            this.tokenizer.quickUpdate(Math.min(offset, offset + delta));
        }
    };

    P._saveExcursion = function(cont, markerBefore) {
        var tmp = this.createMarker(null, markerBefore);
        ++this.__savingExcursion;
        try {
            return cont.call(this);
        } finally {
            --this.__savingExcursion;
            this.caretMarker.swap(tmp, false, true);
            tmp.destroy();
        }
    };

    P._disableUndo = function(cont) {
        ++this.__preventUndo;
        try {
            return cont.call(this);
        } finally {
            --this.__preventUndo;
        }
    };

    P._handleKeyEvent = function(ev) {
        var handled = false;
        this.interactiveEvent(ev);
        var lcwk = this._lastCommandWasKill;

        if (this.__nextIsMeta)
            ev.altKey = true;
        this.__nextIsMeta = false;

        var key = Ymacs_Keymap.unparseKey(ev);
        var cc = this.currentKeys;
        var foundPrefix = false;
        cc.push(key);

        this.keymap.r_foreach(function(km){
            var h = km.getHandler(cc);
            if (h instanceof Array) {
                this.callInteractively(h[0], h[1]);
                handled = true;
            }
            else if (h) {
                handled = foundPrefix = true;
            }
            else if (key === "ESCAPE") {
                this.__nextIsMeta = true;
                handled = true;
            }
            else if (km.defaultHandler && cc.length == 1) {
                handled = this.callInteractively(km.defaultHandler[0], km.defaultHandler[1]);
            }
            if (handled)
                $BREAK();
        }, this);

        if (!foundPrefix) {
            if (!handled) {
                if (cc.length > 1) {
                    this.signalError(cc.join(" ").bold() + " is undefined", true);
                    handled = true;
                }
            }
            cc.splice(0, cc.length);
        }

        if (this._lastCommandWasKill == lcwk && typeof handled != "object" && !this.__nextIsMeta) {
            // selecting a prefix keymap shouldn't clear the killRing
            this._lastCommandWasKill = 0;
        }

        this.callHooks("finishedEvent", handled);
        this.interactiveEvent(null);
        return handled;
    };

    P._on_tokenizerFoundToken = function(row, c1, c2, what) {
        if (what) {
            this._textProperties.addLineProps(row, c1, c2, "css", what);
        } else {
            this._textProperties.removeLineProps(row, c1, c2, "css");
        }
    };

    P._on_textPropertiesChange = function(row) {
        if (this.__preventUpdates == 0) {
            this.callHooks("onLineChange", row);
        } else {
            this.__dirtyLines[row] = true;
        }
    };

    P.formatLineHTML = function(row, caret) {
        var rc = this._rowcol;
        if (caret instanceof Ymacs_Marker)
            rc = caret.getRowCol();
        caret = row == rc.row ? rc.col : null;
        return this._textProperties.getLineHTML(row, this.code[row], caret);
    };

});
