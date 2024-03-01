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

import { DOM, Widget, remove } from "./ymacs-utils.js";

function selectClosestFrameX(byx, pos) {
    if (byx.length > 0) {
        var x = byx.peek().getPos().x, a = [ byx.pop() ];
        while (byx.length > 0 && byx.peek().getPos().x == x)
            a.push(byx.pop());
        return a.minElement(function(f){
            return Math.abs(pos.y - f.getPos().y - f.getSize().y/2);
        });
    }
}

function selectClosestFrameY(byy, pos) {
    if (byy.length > 0) {
        var y = byy.peek().getPos().y, a = [ byy.pop() ];
        while (byy.length > 0 && byy.peek().getPos().y == y)
            a.push(byy.pop());
        return a.minElement(function(f){
            return Math.abs(pos.x - f.getPos().x - f.getSize().x/2);
        });
    }
}

function ensureLocalStorage() {
    if (!(window.localStorage && window.localStorage.getItem))
        throw new Ymacs_Exception("Local storage facility not available in this browser");
}

function isModifier(key) {
    return /^(?:Alt|AltGraph|CapsLock|Control|Fn|FnLock|Hyper|Meta|NumLock|ScrollLock|Shift|Super|Symbol|SymbolLock)$/.test(key);
}

class Ymacs extends Widget {

    static options = {
        buffers       : [],
        frames        : [],
        cf_frameStyle : Object.create(null),
    };

    constructor(...args) {
        super(...args);

        this.buffers = [...this.o.buffers];
        this.frames = [...this.o.frames];
        this.cf_frameStyle = {...this.o.cf_frameStyle};

        this.buffers.forEach(b => {
            b.ymacs = this;
            this._addBufferListeners(b);
        });

        /* -----[ variables ]----- */
        this.killRing = [];
        this.killMasterOfRings = [];
        this.progress = {};

        /* -----[ macro vars ]----- */
        // If present, keystrokes are stored in this list.
        this.__macro_recording = null;
        // This is the macro executed by C-x e and named by
        // name-last-kbd-macro.
        this.__macro_finished = null;
        // Set when any buffer does signalError.  Tells us when to abort
        // running a macro.
        this.__error_thrown = false;
        // A list if we're executing a macro.
        this.__running_macro = null;
        // A number of times to execute the current macro.
        this.__macro_times = 0;
        // Macro current step
        this.__macro_step = 0;
        // Timer for the macro
        this.__macro_timer = null;
        // Unbiased active frame
        this.__input_frame = null;

        /* -----[ minibuffer ]----- */
        this.minibuffer = this.createBuffer({ hidden: true, isMinibuffer: true });
        this.minibuffer.cmd("minibuffer_mode");
        this.minibuffer_frame = this.createFrame({
            isMinibuffer         : true,
            buffer               : this.minibuffer,
            hidden               : true,
            highlightCurrentLine : false,
            className            : "Ymacs_Minibuffer"
        });

        /* -----[ main content ]----- */
        if (this.buffers.length == 0)
            this.createBuffer();

        var frame = this.createFrame({ buffer: this.buffers[0] });

        this.add(frame);
        this.add(this.minibuffer_frame);

        this.setActiveFrame(frame);
        frame._redrawCaret();
    }

    initClassName() {
        return "Ymacs";
    }

    _addBufferListeners(buf) {
        buf.addEventListener("onDestroy", () => {
            var fr = this.getActiveFrame();
            this.getBufferFrames(buf).forEach(f => {
                if (f !== fr) {
                    this.deleteFrame(f);
                }
            });
            remove(this.buffers, buf);
            if (this.getActiveBuffer() === buf)
                this.nextHiddenBuffer(buf);
        });
    }

    pushToKillRing(text, prepend) {
        prepend ? this.killRing.unshift(text)
            : this.killRing.push(text);
    }

    killRingToMaster() {
        if (this.killRing.length && (this.killMasterOfRings.length == 0 ||
                                     this.killMasterOfRings.peek().join("") != this.killRing.join("")))
            this.killMasterOfRings.push(this.killRing);
        this.killRing = [];
    }

    killRingText() {
        return this.killRing.join("");
    }

    rotateKillRing(push) {
        if (push) {
            this.killMasterOfRings.push(this.killRing);
            this.killRing = this.killMasterOfRings.shift();
        } else {
            this.killMasterOfRings.unshift(this.killRing);
            this.killRing = this.killMasterOfRings.pop();
        }
    }

    getBuffer(buf) {
        if (!(buf instanceof Ymacs_Buffer)) {
            buf = this.buffers.find(b => b.name == buf);
        }
        return buf;
    }

    killBuffer(buf) {
        buf = this.getBuffer(buf);
        this.callHooks("onDeleteBuffer", buf);
        buf.destroy();
    }

    renameBuffer(buf, name) {
        buf = this.getBuffer(buf);
        buf.name = name;
        buf.callHooks("onProgressChange");
    }

    _do_switchToBuffer(buf) {
        this.getActiveFrame().setBuffer(buf);
        this.callHooks("onBufferSwitch", buf);
    }

    switchToBuffer(maybeName) {
        var buf = this.getBuffer(maybeName), a = this.buffers;
        if (!buf) {
            // create new buffer
            buf = this.createBuffer({ name: maybeName });
        }
        remove(a, buf);
        a.unshift(buf);
        this._do_switchToBuffer(buf);
        return buf;
    }

    nextHiddenBuffer(cur) {
        var a = this.buffers.filter(buf => {
            if (buf === cur) return false;
            var hidden = true;
            buf.forAllFrames(() => hidden = false);
            return hidden;
        });
        if (a.length > 0) {
            var buf = a[0];
            remove(this.buffers, buf);
            this.buffers.push(buf);
            this._do_switchToBuffer(buf);
        } else {
            this.switchToBuffer("*scratch*");
        }
    }

    switchToNextBuffer() {
        var a = this.buffers;
        if (a.length > 1) {
            var buf = a.shift();
            a.push(buf);
            this._do_switchToBuffer(a[0]);
        }
    }

    switchToPreviousBuffer() {
        var a = this.buffers;
        if (a.length > 1) {
            var buf = a.pop();
            a.unshift(buf);
            this._do_switchToBuffer(buf);
        }
    }

    getNextBuffer(buf, n) {
        if (n == null) n = 1;
        var a = this.buffers;
        return a[(a.indexOf(buf) + n) % a.length];
    }

    getPrevBuffer(buf, n) {
        if (n == null) n = 1;
        return this.getNextBuffer(buf, -n);
    }

    getBufferFrames(buf) {
        buf = this.getBuffer(buf);
        return this.frames.filter(f => f.buffer === buf);
    }

    createBuffer(args) {
        args = Object.assign({}, args, { ymacs: this });
        var buf = new Ymacs_Buffer(args);
        this._addBufferListeners(buf);
        if (!args.hidden)
            this.buffers.push(buf);
        this.callHooks("onCreateBuffer", buf);
        return buf;
    }

    createFrame(args) {
        args = Object.assign({}, args, { ymacs: this });
        var frame = new Ymacs_Frame(args);
        if (!args.hidden)
            this.frames.unshift(frame);
        frame.setStyle(this.cf_frameStyle);
        return frame;
    }

    setFrameStyle(style) {
        [ this.minibuffer_frame, ...this.frames ].forEach(frame => frame.setStyle(style));
    }

    keepOnlyFrame(frame) {
        if (this.frames.length > 1) {
            var el = frame.getElement();
            while (el.parentNode != this.getContentElement())
                el = el.parentNode;
            if (el !== frame) {
                el.replaceWith(frame.getElement());
                frame.getElement().style.removeProperty("width");
                frame.getElement().style.removeProperty("height");
                this.setActiveFrame(frame);
                frame.centerOnCaret();
                this.frames = [ frame ];
            }
        }
    }

    deleteFrame(frame) {
        if (this.frames.length > 1) {
            remove(this.frames, frame);
            let parent = frame.getElement().parentNode;
            let other = [...parent.children].find(el => {
                let obj = el._ymacs_object;
                return obj instanceof Ymacs_SplitCont
                    || (obj instanceof Ymacs_Frame && obj !== frame);
            });
            other.style.removeProperty("width");
            other.style.removeProperty("height");
            parent.replaceWith(other);
            if (!DOM.hasClass(other, "Ymacs_Frame")) {
                other = other.querySelector(".Ymacs_Frame");
            }
            other = other._ymacs_object;
            this.setActiveFrame(other);
            other.centerOnCaret();
        }
    }

    focusOtherFrame() {
        this.setActiveFrame(this.frames[0]);
    }

    focus() {
        this.frames.peek().focus();
    }

    setInputFrame(frame) {
        this.__input_frame = frame;
    }

    setActiveFrame(frame, nofocus) {
        if (!frame.isMinibuffer) {
            var old = this.getActiveFrame();
            if (old) {
                old.delClass("Ymacs_Frame-active");
            }
            remove(this.frames, frame);
            this.frames.push(frame);
        }
        this.__input_frame = frame;
        if (!nofocus)
            frame.focus();
    }

    getActiveFrame() {
        return this.frames.peek();
    }

    getActiveBuffer() {
        var frame = this.getActiveFrame();
        return frame ? frame.buffer : this.buffers.peek();
    }

    setColorTheme(themeId) {
        this.delClass(/Ymacs-Theme-[^\s]*/g);
        if (!(themeId instanceof Array))
            themeId = [ themeId ];
        themeId.forEach(themeId => {
            this.addClass("Ymacs-Theme-" + themeId);
        });
    }

    getFrameInDirection(dir) {
        let frame = this.getActiveFrame();
        let caret = frame.getCaretElement();
        let box = caret.getBoundingClientRect();
        let pos = {
            x: box.left, y: box.top,
            sz: { x: box.width, y: box.height }
        };
        var byx = this.frames.sort((a, b) => a.getPos().x - b.getPos().x);
        var byy = this.frames.sort((a, b) => a.getPos().y - b.getPos().y);
        return this["_get_frameInDir_" + dir](byx, byy, pos, frame);
    }

    _get_frameInDir_left(byx, byy, pos, frame) {
        byx = byx.filter(f => {
            let p = f.getPos(), s = f.getSize();
            return (f !== frame) && (p.x < pos.x) && (p.y - pos.sz.y <= pos.y) && (p.y + s.y > pos.y);
        });
        return selectClosestFrameX(byx, pos);
    }

    _get_frameInDir_right(byx, byy, pos, frame) {
        byx.reverse();
        byx = byx.filter(f => {
            let p = f.getPos(), s = f.getSize();
            return (f !== frame) && (p.x > pos.x) && (p.y - pos.sz.y <= pos.y) && (p.y + s.y > pos.y);
        });
        return selectClosestFrameX(byx, pos);
    }

    _get_frameInDir_up(byx, byy, pos, frame) {
        byy = byy.filter(f => {
            let p = f.getPos(), s = f.getSize();
            return (f !== frame) && (p.y < pos.y) && (p.x - pos.sz.x <= pos.x) && (p.x + s.x > pos.x);
        });
        return selectClosestFrameY(byy, pos);
    }

    _get_frameInDir_down(byx, byy, pos, frame) {
        byy.reverse();
        byy = byy.filter(f => {
            let p = f.getPos(), s = f.getSize();
            return (f !== frame) && (p.y > pos.y) && (p.x - pos.sz.x <= pos.x) && (p.x + s.x > pos.x);
        });
        return selectClosestFrameY(byy, pos);
    }

    /* -----[ local storage ]----- */

    ls_get() {
        ensureLocalStorage();
        return DlJSON.decode(localStorage.getItem(".ymacs") || "{}", true);
    }

    ls_set(src) {
        ensureLocalStorage();
        localStorage.setItem(".ymacs", DlJSON.encode(src));
    }

    ls_getFileContents(name, nothrow) {
        var info = this.ls_getFileDirectory(name), other = info.other, code;
        if (other.length == 1) {
            code = info.dir[other[0]];
        }
        if (code == null && !nothrow) {
            throw new Ymacs_Exception("File not found");
        }
        return code;
    }

    ls_setFileContents(name, content) {
        var files = this.ls_getFileDirectory(name, "file");
        files.dir[files.other[0]] = content;
        this.ls_set(files.store);
    }

    ls_getFileDirectory(name, create) {
        var store, dir = store = this.ls_get(), back = [];
        name = name.replace(/^[~\x2f]+/, "").split(/\x2f+/);
        var path = [], other = [];
        while (name.length > 0) {
            var part = name.shift();
            if (part == ".") continue;
            if (part == "..") {
                path.pop();
                dir = back.pop();
            }
            else if (part == "~") {
                path = [];
                other = [];
                back = [];
                dir = store;
            }
            else if (dir.hasOwnProperty(part) && (typeof dir[part] != "string")) {
                back.push(dir);
                dir = dir[part];
                path.push(part);
            }
            else {
                other.push(part);
            }
        }
        if (create) {
            var n = create == "file" ? 1 : 0;
            while (other.length > n) {
                dir = dir[other.shift()] = {};
            }
            this.ls_set(store);
        }
        return {
            store : store,
            dir   : dir,
            path  : path,
            other : other,
            full  : path.concat(other).join("/")
        };
    }

    ls_deleteFile(name) {
        var info = this.ls_getFileDirectory(name);
        delete info.dir[info.other.join("/")];
        this.ls_set(info.store);
    }

    /* -----[ filesystem operations ]----- */

    fs_normalizePath(path) {
        path = path.replace(/^[~\x2f]+/, "").split(/\x2f+/);
        var ret = [];
        while (path.length > 0) {
            var x = path.shift();
            if (x != ".") {
                if (x == "..") {
                    ret.pop();
                } else if (x == "~") {
                    ret = [];
                } else {
                    ret.push(x);
                }
            }
        }
        return ret.join("/");
    }

    fs_fileType(name, cont) {
        try {
            this.ls_getFileContents(name);
            cont(true);
        } catch(ex) {
            cont(null);
        }
    }

    fs_getFileContents(name, nothrow, cont) {
        var code = this.ls_getFileContents(name, nothrow);
        cont(code, code); // second parameter is file stamp, on a real fs it should be last modification time
    }

    fs_setFileContents(name, content, stamp, cont) {
        if (stamp && (this.ls_getFileContents(name, true) || "") != stamp) {
            cont(null); // did not change file because stamp is wrong
        } else {
            this.ls_setFileContents(name, content);
            cont(content);
        }
    }

    fs_getDirectory(dirname, cont) {
        var info = this.ls_getFileDirectory(dirname, false);
        dirname = info.path.join("/"); // normalized
        if (info) {
            var files = {};
            for (var f in info.dir) {
                if (Object.HOP(info.dir, f)) {
                    files[f] = {
                        name : f,
                        path : dirname + "/" + f,
                        type : typeof info.dir[f] == "string" ? "regular" : "directory"
                    };
                }
            }
            cont(files);
        } else {
            cont(null);
        }
    }

    fs_deleteFile(name, cont) {
        this.ls_deleteFile(name);
        cont();
    }

    fs_remapDir(dir, cont) {
        cont(dir);
    }

    isRunningMacro() {
        return !!this.__running_macro;
    }

    isRecordingMacro() {
        return !!this.__macro_recording;
    }

    indicateError() {
        this.__error_thrown = true;
    }

    startMacro(do_append) {
        if (this.isRecordingMacro())
            return false;
        if (do_append) {
            this.__macro_recording = this.__macro_finished || [];
            this.__macro_finished = null;
        } else
            this.__macro_recording = [];
        return true;
    }

    stopMacro() {
        if (this.__macro_recording) {
            this.__macro_finished = this.__macro_recording;
            this.__macro_recording = null;
        }
    }

    getLastMacro() {
        return this.__macro_finished;
    }

    stepMacro() {
        while (true) {
            if (this.__macro_step >= this.__running_macro.length) {
                this.__macro_times--;
                this.__macro_step = 0;
            }
            if (this.__macro_times == 0 || this.__error_thrown) {
                this.__macro_times = 0;
                this.__macro_step = 0;
                this.__running_macro = null;
                return;
            }
            var ev = this.__running_macro[this.__macro_step];
            this.processKeyEvent(ev, ev.wasKeypress);
            this.__macro_step++;
        }
    }

    runMacro(times, macro) {
        if (this.isRecordingMacro())
            return false;
        this.__error_thrown = false;
        this.__running_macro = macro;
        this.__macro_step = 0;
        this.__macro_times = times;
        var self = this;
        setTimeout(function() { self.stepMacro(); }, 0);
        return true;
    }

    processKeyEvent(ev, press) {
        var frame = this.__input_frame;
        var buffer = frame.buffer;

        ev.wasKeypress = press;

        if (press) {
            if (this.__macro_recording) {
                this.__macro_recording.push(ev);
            }
            return buffer._handleKeyEvent(ev);
        } else {
            if (isModifier(ev.key) || ev.key.length == 1 && !(ev.ctrlKey || ev.altKey)) {
                return false; // to be handled by the upcoming keypress event
            }
            if (this.__macro_recording) {
                this.__macro_recording.push(ev);
            }
            return buffer._handleKeyEvent(ev);
        }
    }
}

window.Ymacs = Ymacs; // XXX.
