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

import { Ymacs_Marker } from "./ymacs-marker.js";
import { DOM, EventProxy, remove, delayed, formatBytes, backward_regexp } from "./ymacs-utils.js";
import { Ymacs_Keymap } from "./ymacs-keymap.js";
import { Ymacs_Keymap_Emacs, Ymacs_Keymap_Minibuffer } from "./ymacs-keymap-emacs.js";
import { Ymacs_Text_Properties } from "./ymacs-textprop.js";
import { Ymacs_Exception } from "./ymacs-exception.js";
import { Ymacs_Interactive } from "./ymacs-interactive.js";

let GLOBAL_VARS = {
    case_fold_search            : true,
    line_movement_requested_col : 0,
    fill_column                 : 78,
    tab_width                   : 8,
    indent_level                : 4,
    sticky_mark                 : false,

    // syntax variables
    syntax_word                 : /^[0-9\p{L}]$/u,
    syntax_word_dabbrev         : /^[0-9_$\p{L}]$/u,
    syntax_paragraph_sep        : /\n\s*\n/g
};

const MAX_UNDO_RECORDS = 50000; // XXX: should we not limit?

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
        var changed = Object.create(null);
        for (var i in key) {
            changed[i] = this[i];
            setq.call(this, i, key[i]);
        }
        return changed;
    }
}

function MRK(x) {
    return x instanceof Ymacs_Marker ? x.getPosition() : x;
}

export class Ymacs_Buffer extends EventProxy {

    static options = {
        name         : "*scratch*",
        code         : "",
        ymacs        : null,
        tokenizer    : null,
    };

    static COMMANDS = Object.create(null);

    static newCommands(...args) {
        return setq.apply(this.COMMANDS, args);
    }

    static replaceCommands(cmds) {
        this.COMMANDS = Object.assign(Object.create(null), this.COMMANDS);
        let replacements = Object.create(null);
        Object.keys(cmds).forEach(oldcmd => {
            let newcmd = cmds[oldcmd];
            if (typeof newcmd == "string") {
                newcmd = this.COMMANDS[newcmd];
            }
            replacements[oldcmd] = newcmd;
        });
        return this.newCommands(replacements);
    }

    static newMode(name, activate) {
        let modevar = "*" + name + "*", hookvar = modevar + "hooks";
        Ymacs_Buffer.setGlobal(hookvar, []);
        this.COMMANDS[name] = Ymacs_Interactive("P", function(force){
            let status = this.getq(modevar);
            if (status) {
                // currently active
                if (force !== true) {
                    // deactivate
                    this.getq(hookvar).forEach(hook => hook.call(this, false));
                    if (status instanceof Function) {
                        // clean-up
                        status.call(this);
                    }
                    this.setq(modevar, null);
                    remove(this.modes, name);
                }
            }
            else {
                // inactive
                if (force !== false) {
                    let off = activate.apply(this, arguments);
                    if (!(off instanceof Function))
                        off = true;
                    this.setq(modevar, off);
                    this.modes.push(name);
                    this.getq(hookvar).forEach(hook => hook.call(this, true));
                }
            }
            return status;
        });
    }

    static getVariable(key) {
        return GLOBAL_VARS[key];
    }

    static setVariable() {
        return setq.apply(GLOBAL_VARS, arguments);
    }

    constructor(...args) {
        super(...args);

        this.isMinibuffer = this instanceof Ymacs_Minibuffer;
        this.COMMANDS = Object.assign(Object.create(null), this.COMMANDS);
        this.name = this.o.name;
        this.ymacs = this.o.ymacs;
        this.tokenizer = this.o.tokenizer;

        this.__savingExcursion = 0;
        this.__preventUpdates = 0;
        this.__preventUndo = 0;
        this.__undoInProgress = 0;
        this.__dirtyLines = [];
        this.__undoQueue = [];
        this.__undoPointer = 0;

        this.markers = [];
        this.caretMarker = this.createMarker(0, false, "point");
        this.markMarker = this.createMarker(0, true, "mark");
        this.matchData = [];
        this.previousCommand = null;
        this.currentCommand = null;
        this.currentKeys = [];
        this.progress = Object.create(null);

        this.variables = Object.create(null);
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
            "onFoundToken": this._on_tokenizerFoundToken.bind(this)
        };

        this._textProperties = new Ymacs_Text_Properties({ buffer: this });
        this._textProperties.addEventListener("onChange", this._on_textPropertiesChange.bind(this));

        this.keymap = [];
        this.pushKeymap(this.makeDefaultKeymap());
        this.setCode(this.o.code);
        this._lastCommandWasKill = 0;
    }

    addModeHook(name, func) {
        if (typeof func == "string")
            func = this.COMMANDS[func];
        let hookvar = "*" + name + "*hooks";
        this.getq(hookvar).pushUnique(func);
    }

    removeModeHook(name, func) {
        if (typeof func == "string")
            func = this.COMMANDS[func];
        let hookvar = "*" + name + "*hooks";
        remove(this.getq(hookvar), func);
    }

    withVariables(vars, cont) {
        var saved = this.variables;
        this.variables = Object.assign(Object.create(this.variables), vars);
        try {
            if (cont instanceof Function) {
                return cont.apply(this, [...arguments].slice(2));
            } else {
                return this.cmdApply(cont, [...arguments].slice(2));
            }
        } finally {
            this.variables = saved;
        }
    }

    withCommands(cmds, cont) {
        var saved = this.COMMANDS;
        this.COMMANDS = Object.assign(Object.create(this.COMMANDS), cmds);
        try {
            if (cont instanceof Function) {
                return cont.apply(this, [...arguments].slice(2));
            } else {
                return this.cmdApply(cont, [...arguments].slice(2));
            }
        } finally {
            this.COMMANDS = saved;
        }
    }

    getVariable(name) {
        return (name in this.variables)
            ? this.variables[name]
            : GLOBAL_VARS[name];
    }

    setVariable() {
        return setq.apply(this.variables, arguments);
    }

    /* -----[ public API ]----- */

    lastIndexOfRegexp(str, re, caret, bound) {
        str = str.substring(0, caret);
        re = backward_regexp(re);
        re.lastIndex = bound || 0;
        var m = re.exec(str);
        if (m) {
            var a = [...m].slice(2);
            a.index = m.index + m[1].length;
            a.after = m.index + m[0].length;
            a[0] = str.substring(a.index, a.after);
            this.matchData = a;
            return a;
        }
    }

    pushKeymap(keymap) {
        if (keymap instanceof Array) {
            keymap.forEach(this.pushKeymap, this);
        } else {
            this.popKeymap(keymap);
            this.keymap.push(keymap);
            keymap.attached(this);
        }
    }

    popKeymap(keymap) {
        remove(this.keymap, keymap);
        keymap.detached(this);
    }

    makeDefaultKeymap() {
        return this.isMinibuffer ? Ymacs_Keymap_Minibuffer : Ymacs_Keymap_Emacs;
    }

    signalError(text, isHtml, timeout) {
        // Terminates a running macro
        this.ymacs.indicateError();
        this.callHooks("onMessage", { type: "error", text, isHtml, timeout });
    }

    signalInfo(text, isHtml, timeout) {
        this.callHooks("onMessage", { type: "info", text, isHtml, timeout });
    }

    popupMessage(args) {
        this.callHooks("onMessage", args);
    }

    createMarker(pos, before, name) {
        if (pos == null)
            pos = this.point();
        return new Ymacs_Marker({ editor: this, pos: pos, name: name, before: before });
    }

    point() {
        return this.caretMarker.getPosition();
    }

    dirty(dirty) {
        if (arguments.length > 0) {
            this.__isDirty = dirty;
            this.__undoQueue.forEach(x => {
                if (x.type !== 3) x.dirty = true;
            });
            this.updateModeline();
        }
        return this.__isDirty;
    }

    setCode(code) {
        // this.__code = code = code.replace(/\t/g, " ".repeat(this.getq("tab_width")));
        this.__isDirty = false;
        this.__code = code;
        this.__size = code.length;
        this.__undoQueue = [];
        this.__undoPointer = 0;
        this.markers.map(m => m.setPosition(0, true, true));
        this.code = code.split(/\n/);
        this._textProperties.reset();
        if (this.tokenizer) {
            this.tokenizer.reset();
        }
        this.callHooks("onResetCode", this.code);
        this.caretMarker.setPosition(0, false, true);
        this.markMarker.setPosition(0, true);
        this.forAllFrames(frame => {
            frame.ensureCaretVisible();
            frame.redrawModelineWithTimer();
        });
    }

    setTokenizer(tok) {
        if (this.tokenizer != null) {
            this.tokenizer.removeEventListener(this._tokenizerEvents);
        }
        this.tokenizer = tok;
        if (tok) {
            tok.addEventListener(this._tokenizerEvents);
            tok.reset();
        } else {
            this._textProperties.reset();
            this.callHooks("onResetCode", this.code);
        }
    }

    getCode() {
        return this.__code || (this.__code = this.code.join("\n"));
    }

    getCodeSize() {
        if (this.__size)
            return this.__size;
        var i = this.code.length, size = i > 0 ? -1 : 0;
        while (--i >= 0)
            size += this.code[i].length + 1;
        return this.__size = size;
    }

    getLine(row) {
        if (row == null)
            row = this._rowcol.row;
        return this.code[row];
    }

    charAtRowCol(row, col) {
        var n = this.code.length;
        if (row >= n--)
            return null;
        var line = this.code[row];
        if (col == line.length)
            return row == n && line.charAt(col) || "\n";
        return line.charAt(col);
    }

    charAt(point) {
        if (point == null)
            point = this.point();
        else {
            point = MRK(point);
            if (point < 0)
                point += this.point();
        }
        var rc = this._positionToRowCol(point);
        return this.charAtRowCol(rc.row, rc.col);
    }

    callInteractively(func, args, finalArgs) {
        if (!args) args = [];
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
            if (!func.ymacsMarkExtend && !this.getq("sticky_mark")) {
                this.clearTransientMark();
            }
            return func.apply(this, args);
        } catch(ex) {
            if (ex instanceof Ymacs_Exception) {
                this.signalError(ex.message);
            } else {
                throw ex;
            }
        } finally {
            if (this.getq("sticky_mark")) {
                this.ensureTransientMark();
            }
            if (cmd != "undo") {
                this.__undoPointer = this.__undoQueue.length;
            }
            this.resumeUpdates();
            this.callHooks("afterInteractiveCommand", cmd, func);
            this.previousCommand = cmd;
            this.sameCommandCount(+1);
        }
    }

    resetOverwriteMode(om) {
        if (arguments.length == 0)
            om = this.overwriteMode;
        this.callHooks("onOverwriteMode", this.overwriteMode = !om);
        this.signalInfo(om ? "Insert mode" : "Overwrite mode");
    }

    getMinibuffer() {
        return this.whenYmacs(function(ymacs) { return ymacs.minibuffer; });
    }

    getMinibufferFrame() {
        return this.whenYmacs(function(ymacs) { return ymacs.minibuffer_frame; });
    }

    setMinibuffer(text) {
        this.whenMinibuffer(function(mb){
            mb.setCode(text);
            mb.cmd("end_of_buffer");
        });
    }

    cmd(cmd, ...args) {
        return this.COMMANDS[cmd].apply(this, args);
    }

    cmdApply(cmd, args) {
        return this.COMMANDS[cmd].apply(this, args);
    }

    createDialog(args) {
        if (!args.parent) {
            args.parent = this.getActiveFrame() && this.getActiveFrame().getParentDialog();
            if (!("noShadows" in args)) {
                args.noShadows = true;
            }
        }
        var dlg = new DlDialog(args);
        this.whenActiveFrame(function(frame){
            dlg.addEventListener("onDestroy", delayed(frame.focus.bind(frame)));
        });
        return dlg;
    }

    getActiveFrame() {
        return this.whenYmacs("getActiveFrame");
    }

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
    when(what, cont, ...args) {
        what = this[what] || this.getq(what);
        if (what != null) {
            if (cont instanceof Function)
                return cont.call(this, what);
            else {
                return what[cont].apply(what, args);
            }
        }
    }

    // XXX: this is way too ugly. (15 years later: Oh Yeah.)
    whenActiveFrame() {
        var fr = this.getActiveFrame(); // miserable hack
        if (fr.buffer === this) {
            this.activeFrame = fr;
            var a = ["activeFrame", ...arguments];
            return this.when.apply(this, a);
        } else {
            this.activeFrame = null;
        }
    }

    forAllFrames(cont) {
        if (this.ymacs)
            this.ymacs.getBufferFrames(this).forEach(cont);
    }

    whenYmacs() {
        var a = ["ymacs", ...arguments];
        return this.when.apply(this, a);
    }

    whenMinibuffer(cont) {
        // In fact, we should move when() into some base
        // object... but which one?  JS doesn't have multiple
        // inheritance, though we could easily "invent" it.
        return this.whenYmacs(function(ymacs){
            if (ymacs.minibuffer)
                return cont.call(this, ymacs.minibuffer);
        });
    }

    preventUpdates() {
        ++this.__preventUpdates;
    }

    resumeUpdates() {
        if ((this.__preventUpdates = Math.max(this.__preventUpdates - 1, 0)) == 0) {
            this.redrawDirtyLines();
        }
    }

    getRegion(begin, end) {
        if (begin == null) begin = this.caretMarker;
        if (end == null) end = this.markMarker;
        begin = MRK(begin);
        end = MRK(end);
        if (end < begin) { var tmp = begin; begin = end; end = tmp; }
        return { begin: begin, end: end };
    }

    redrawDirtyLines() {
        this.callHooks("beforeRedraw");
        this.__dirtyLines.forEach((draw, row) => {
            if (draw) {
                this.callHooks("onLineChange", row);
            }
        });
        this.__dirtyLines = [];
        this.callHooks("afterRedraw");
    }

    setOverlay(name, props) {
        this.callHooks("onOverlayChange", name, props);
    }

    deleteOverlay(name) {
        this.callHooks("onOverlayDelete", name);
    }

    setMark(pos) {
        this.markMarker.setPosition(pos);
    }

    ensureTransientMark() {
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
    }

    clearTransientMark() {
        if (this.transientMarker) {
            this.transientMarker.destroy();
            this.transientMarker = null;
            this.deleteOverlay("selection");
            this.setq("sticky_mark", false);
        }
    }

    deleteTransientRegion() {
        if (this.transientMarker) {
            this._deleteText(this.caretMarker, this.transientMarker);
            this.clearTransientMark();
            this._placeUndoBoundary();
            return true;
        }
    }

    static #sameCommandCount = 0;
    sameCommandCount(diff) {
        if (diff == null) return Ymacs_Buffer.#sameCommandCount;
        if (diff == 0) return Ymacs_Buffer.#sameCommandCount = 0;
        return Ymacs_Buffer.#sameCommandCount += diff;
    }

    static #lastKeyEvent = null;
    interactiveEvent(ev) {
        if (arguments.length == 0)
            return Ymacs_Buffer.#lastKeyEvent;
        return Ymacs_Buffer.#lastKeyEvent = ev;
    }

    getPrefixArg(noDiscard) {
        var ret = this.getq("universal_prefix");
        if (!noDiscard && ret !== undefined) {
            this.setq("universal_prefix", undefined);
            if (!this.isMinibuffer)
                this.setMinibuffer("");
        }
        return ret;
    }

    setPrefixArg(val) {
        return this.setq("universal_prefix", val);
    }

    updateProgress(name, val) {
        if (val == null)
            delete this.progress[name];
        else
            this.progress[name] = val;
        this.callHooks("onProgressChange");
    }

    updateModeline() {
        this.callHooks("onProgressChange");
    }

    renderModelineContent(rc, percent) {
        var ml = (this.__isDirty ? "**" : "--") +
            ` <span class="mode-line-buffer-id">${DOM.htmlEscape(this.name)}</span>` +
            "  " + percent + " of " + formatBytes(this.getCodeSize(), 2).toLowerCase() + "  " +
            "(" + (rc.row + 1) + "," + rc.col + ") ";
        var custom = this.getq("modeline_custom_handler");
        if (custom) {
            custom = custom.call(this, this, rc);
            if (custom) ml += "[" + custom + "] ";
        }
        var pr = [];
        for (var i in this.progress) {
            pr.push(i + ": " + this.progress[i]);
        }
        if (pr.length > 0) {
            ml += "{" + pr.join(", ") + "}";
        }
        return ml;
    }

    /* -----[ not-so-public API ]----- */

    // BEGIN: undo queue

    _recordChange(type, pos, len, text) {
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
    }

    _placeUndoBoundary() {
        var q = this.__undoQueue;
        var m = this.markers.map(m => [ m, m.getPosition() ]);
        var last = q.at(-1);
        if (!last || last.type != 3) {
            q.push({ type: 3, markers: m });
        } else {
            last.markers = m;
        }
    }

    _playbackUndo() {
        var q = this.__undoQueue;
        if (q.length == 0) return false;
        ++this.__undoInProgress;
        var didit = false, action;
        while (--this.__undoPointer >= 0) {
            action = q[this.__undoPointer];
            if (action.type == 3) { // boundary
                // restore markers
                action.markers.forEach(m => m[0].setPosition(m[1]));
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
    }

    // END: undo

    _replaceLine(row, text) {
        this.code[row] = text;
        this._textProperties.replaceLine(row, text);
        if (this.__preventUpdates == 0) {
            this.callHooks("onLineChange", row);
        } else {
            this.__dirtyLines[row] = true;
        }
    }

    _deleteLine(row) {
        this.code.splice(row, 1);
        this._textProperties.deleteLine(row);
        if (this.tokenizer)
            this.tokenizer.quickDeleteLine(row);
        this.__dirtyLines.splice(row, 1);
        this.callHooks("onDeleteLine", row);
    }

    _insertLine(row, text) {
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
    }

    _insertText(text, pos) {
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
            for (let j = 0; j < text.length; ++j) {
                this._insertLine(i + j, "");
            }
        } else {
            var lines = text.split("\n"), ln = this.code[i], rest = ln.substr(rc.col);
            if (lines.length > 1) {
                this._replaceLine(i, ln.substr(0, rc.col) + lines.shift());
                lines.forEach(text => this._insertLine(++i, text));
                this._replaceLine(i, this.code[i] + rest);
            } else {
                this._replaceLine(i, ln.substr(0, rc.col) + lines[0] + ln.substr(rc.col));
            }
        }
        this._updateMarkers(pos, text.length);
        this.callHooks("onTextInsert", pos, text);
    }

    _deleteText(begin, end) {
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
            for (let n = erc.row - brc.row; n-- > 0;) this._deleteLine(line);
        }
        this._updateMarkers(begin, begin - end, begin);
        this.callHooks("onTextDelete", begin, end);
    }

    _replaceText(begin, end, text) {
        this._deleteText(begin, end);
        this._insertText(text, begin);
    }

    _swapAreas(a) {
        a = a.map(MRK).sort();
        var b1 = a[0];
        var e1 = a[1];
        var b2 = a[2];
        var e2 = a[3];
        var t1 = this._bufferSubstring(b1, e1);
        var t2 = this._bufferSubstring(b2, e2);
        this._replaceText(b2, e2, t1);
        this._replaceText(b1, e1, t2);
        return e2;
    }

    _bufferSubstring(begin, end) {
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
    }

    _killingAction(p1, p2, prepend, noDelete) {
        p1 = MRK(p1);
        p2 = MRK(p2);
        var text = this._bufferSubstring(p1, p2);
        this._saveKilledText(text, prepend);
        if (!noDelete)
            this._deleteText(p1, p2);
        this.clearTransientMark();
    }

    _saveKilledText(text, prepend) {
        if (!this._lastCommandWasKill)
            this.ymacs.killRingToMaster();
        this.ymacs.pushToKillRing(text, prepend);
        this._lastCommandWasKill++;
        if (this.interactiveEvent()) try {
            navigator.clipboard.writeText(this.ymacs.killRingText());
        } catch {}
    }

    _positionToRowCol(pos) {
        var line = 0, a = this.code, n = a.length;
        while (pos > 0 && line < n) {
            var len = a[line].length;
            if (len >= pos)
                break;
            pos -= len + 1; // one for the newline
            line++;
        }
        return { row: line, col: pos };
    }

    _rowColToPosition(row, col) {
        var pos = 0, a = this.code, i = Math.min(row, a.length - 1), n = i;
        if (i < 0)
            return 0;
        while (--i >= 0)
            pos += a[i].length + 1; // one for the newline
        return pos + Math.min(col, a[n].length);
    }

    _boundPosition(pos) {
        if (pos < 0)
            return 0;
        return Math.min(pos, this.getCodeSize());
    }

    _repositionCaret(pos) {
        var p = this.caretMarker.getPosition();
        if (pos == null)
            pos = p;
        pos = MRK(pos);
        pos = this._boundPosition(pos);
        this.caretMarker.setPosition(pos);
        return pos != p;
    }

    _updateMarkers(offset, delta, min) {
        this.__size = null;
        this.__code = null;
        // if (this.__undoInProgress == 0) {
        this.markers.map(m => m.editorChange(offset, delta, min || 0));
        // }
        if (this.tokenizer) {
            this.tokenizer.quickUpdate(Math.min(offset, offset + delta));
        }
    }

    _saveExcursion(cont, markerBefore) {
        var tmp = this.createMarker(null, markerBefore);
        ++this.__savingExcursion;
        try {
            return cont.call(this);
        } finally {
            --this.__savingExcursion;
            this.caretMarker.swap(tmp, false, true);
            tmp.destroy();
        }
    }

    _disableUndo(cont) {
        ++this.__preventUndo;
        try {
            return cont.call(this);
        } finally {
            --this.__preventUndo;
        }
    }

    _handleKeyEvent(ev) {
        var handled = false;
        this.interactiveEvent(ev);
        var lcwk = this._lastCommandWasKill;

        if (this.__nextIsMeta)
            ev.ymacsMeta = true;
        this.__nextIsMeta = false;

        var key = Ymacs_Keymap.unparseKey(ev);
        var cc = this.currentKeys;
        var foundPrefix = false;
        cc.push(key);

        for (let i = this.keymap.length; --i >= 0;) {
            let km = this.keymap[i];
            let h = km.getHandler(cc);
            if (h instanceof Array) {
                this.callInteractively(h[0], h[1]);
                handled = true;
            }
            else if (h) {
                handled = foundPrefix = true;
            }
            else if (km.defaultHandler && cc.length == 1) {
                handled = this.callInteractively(km.defaultHandler[0], km.defaultHandler[1]);
            }
            if (handled)
                break;
        }

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

        // XXX: always preventDefault() in minibuffer (return true
        // here); seems good, objections?
        return handled || this.isMinibuffer;
    }

    _on_tokenizerFoundToken(row, c1, c2, what) {
        if (what) {
            this._textProperties.addLineProps(row, c1, c2, "css", what);
        } else {
            this._textProperties.removeLineProps(row, c1, c2, "css");
        }
    }

    _on_textPropertiesChange(row) {
        if (this.__preventUpdates == 0) {
            this.callHooks("onLineChange", row);
        } else {
            this.__dirtyLines[row] = true;
        }
    }

    formatLineHTML(row, caret) {
        var rc = this._rowcol;
        if (caret instanceof Ymacs_Marker)
            rc = caret.getRowCol();
        caret = row == rc.row ? rc.col : null;
        return this._textProperties.getLineHTML(row, this.code[row], caret);
    }

    looking_at(needle) {
        var haystack = this.getCode();
        if (needle instanceof RegExp) {
            var pos = needle.lastIndex = this.point();
            var ret = needle.exec(haystack);
            if (ret && ret.index == pos) {
                ret.after = needle.lastIndex;
                return this.matchData = ret;
            }
        } else if (typeof needle == "string") {
            // XXX: account for case_fold_search here?
            return haystack.substr(this.point(), needle.length) == needle;
        }
    }

    looking_back(needle) {
        var haystack = this.getCode();
        if (needle instanceof RegExp) {
            var m = this.lastIndexOfRegexp(haystack, needle, this.point());
            if (m && m.after == this.point())
                return m;
        } else if (typeof needle == "string") {
            // XXX: account for case_fold_search here?
            return haystack.substr(this.point() - needle.length, needle.length) == needle;
        }
    }
}

// XXX: what a mess..
Ymacs_Buffer.prototype.COMMANDS = Ymacs_Buffer.COMMANDS;
Ymacs_Buffer.prototype.newCommands = Ymacs_Buffer.newCommands;
Ymacs_Buffer.prototype.replaceCommands = Ymacs_Buffer.replaceCommands;
Ymacs_Buffer.prototype.newMode = Ymacs_Buffer.newMode;
Ymacs_Buffer.prototype.addModeHook = Ymacs_Buffer.addModeHook;
Ymacs_Buffer.prototype.removeModeHook = Ymacs_Buffer.removeModeHook;
Ymacs_Buffer.setq =
    Ymacs_Buffer.setGlobal =
    Ymacs_Buffer.prototype.setGlobal = Ymacs_Buffer.setVariable;
Ymacs_Buffer.prototype.setq = Ymacs_Buffer.prototype.setVariable;
Ymacs_Buffer.prototype.getq = Ymacs_Buffer.prototype.getVariable;
Ymacs_Buffer.getq = Ymacs_Buffer.getVariable;

export class Ymacs_Minibuffer extends Ymacs_Buffer {
    constructor(...args) {
        super(...args);
        this.promptMarker = this.createMarker(0, true, "prompt");
        this.setq("minibuffer_validation", whatever => true);
    }

    prompt(text) {
        this.setCode(text.trim() + " ");
        this._textProperties.addLineProps(0, 0, text.length - 1, "css", "minibuffer-prompt");
        this._repositionCaret(text.length);
        this.promptMarker.setPosition(text.length, true, true);
    }

    _boundPosition(pos) {
        return Math.max(MRK(this.promptMarker),
                        Math.min(pos, this.getCodeSize()));
    }
}
