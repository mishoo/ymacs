/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT

import { DOM, Widget, remove } from "./ymacs-utils.js";
import { Ymacs_Buffer, Ymacs_Minibuffer } from "./ymacs-buffer.js";
import { Ymacs_Popup } from "./ymacs-popup.js";
import { Ymacs_Frame, Ymacs_SplitCont } from "./ymacs-frame.js";
import { Ymacs_Exception } from "./ymacs-exception.js";

function minElement(array, f, obj, remove) {
    if (array.length == 0)
        return null;
    var i = 0, minEl = array[0], minValue = f.call(obj, minEl), minIndex = 0, tmp;
    while (++i < array.length) if ((tmp = f.call(obj, array[i])) < minValue) {
        minValue = tmp;
        minIndex = i;
        minEl = array[i];
    }
    if (remove)
        array.splice(minIndex, 1);
    return minEl;
}

function selectClosestFrameX(byx, pos) {
    if (byx.length > 0) {
        var x = byx.at(-1).getBox().left, a = [ byx.pop() ];
        while (byx.length > 0 && byx.at(-1).getBox().left == x)
            a.push(byx.pop());
        return minElement(a, function(f){
            return Math.abs(pos.top - f.getBox().top - f.getBox().height/2);
        });
    }
}

function selectClosestFrameY(byy, pos) {
    if (byy.length > 0) {
        var y = byy.at(-1).getBox().top, a = [ byy.pop() ];
        while (byy.length > 0 && byy.at(-1).getBox().top == y)
            a.push(byy.pop());
        return minElement(a, function(f){
            return Math.abs(pos.left - f.getBox().left - f.getBox().width/2);
        });
    }
}

function ensureLocalStorage() {
    if (!(window.localStorage && window.localStorage.getItem))
        throw new Ymacs_Exception("Local storage facility not available in this browser");
}

export class Ymacs extends Widget {

    static options = {
        buffers       : [],
        frames        : [],
        cf_frameStyle : Object.create(null),
        ls_keyName    : ".ymacs",
    };

    constructor(...args) {
        super(...args);

        this.buffers = [...this.o.buffers];
        this.frames = [...this.o.frames];
        this.registers = Object.create(null);
        this.cf_frameStyle = {...this.o.cf_frameStyle};

        this.buffers.forEach(b => {
            b.ymacs = this;
            this._addBufferListeners(b);
        });

        /* -----[ variables ]----- */
        this.killRing = [];
        this.killMasterOfRings = [];

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
        this.minibuffer = new Ymacs_Minibuffer({ ymacs: this });
        this.minibuffer.cmd("minibuffer_mode");
        this.minibuffer_frame = this.createFrame({
            isMinibuffer : true,
            buffer       : this.minibuffer,
            hidden       : true,
        });

        /* -----[ main content ]----- */
        if (this.buffers.length == 0)
            this.createBuffer();

        var frame = this.createFrame({ buffer: this.buffers[0] });

        this.add(frame);
        this.add(this.minibuffer_frame);

        this.setActiveFrame(frame);
        frame.redrawCaret();
    }

    initClassName() {
        return "Ymacs Ymacs-cursor-block";
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
                                     this.killMasterOfRings.at(-1).join("") != this.killRing.join("")))
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
        buf.callHooks("onModelineChange");
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

    createBuffer(args = {}) {
        var buf = new Ymacs_Buffer({ ...args, ymacs: this });
        this._addBufferListeners(buf);
        if (!args.hidden)
            this.buffers.push(buf);
        this.callHooks("onCreateBuffer", buf);
        return buf;
    }

    createFrame(args = {}) {
        var frame = new Ymacs_Frame({ ...args, ymacs: this });
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
            let el = frame.getElement();
            while (el.parentNode != this.getContentElement())
                el = el.parentNode;
            if (el !== frame) {
                el.replaceWith(frame.getElement());
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

    getMainElement() {
        const selector = `:scope > .Ymacs_Frame:not(.Ymacs_Minibuffer), :scope > .Ymacs_SplitCont`;
        return this.getElement().querySelector(selector);
    }

    getFrameConfig() {
        let dig = (el) => {
            if (DOM.hasClass(el, "Ymacs_Frame")) {
                let frame = el._ymacs_object;
                let div = frame.getOverlaysContainer();
                return {
                    buffer: frame.buffer.name,
                    point: +frame.caretMarker,
                    scroll: div.scrollTop / div.scrollHeight,
                    active: frame === this.getActiveFrame(),
                };
            } else {
                let layout = el._ymacs_object;
                let horiz = layout.o.horiz;
                let style = window.getComputedStyle(el);
                let rx = /^\s*(\d*(?:\.\d+)?)px\s+(?:\d*(?:\.\d+)?)px\s+(\d*(?:\.\d+)?)px\s*$/;
                let m = rx.exec(horiz ? style.gridTemplateRows : style.gridTemplateColumns);
                let frac = +m[1] / (+m[1] + +m[2]);
                return [ horiz, frac, dig(el.children[0]), dig(el.children[2]) ];
            }
        };
        return dig(this.getMainElement());
    }

    setFrameConfig(config) {
        this.getMainElement()?.remove();

        // 1. figure out if we already have some frames for the set of
        // buffers that we'll need to switch to.
        let buffers = [];
        let frames = [ ...this.frames ];
        (function dig(el){
            if (Array.isArray(el)) {
                dig(el[2]);
                dig(el[3]);
            } else {
                for (let i = frames.length; --i >= 0;) {
                    if (frames[i].buffer?.name == el.buffer) {
                        buffers.push([ el.buffer, frames[i] ]);
                        frames.splice(i, 1);
                        break;
                    }
                }
            }
        })(config);

        let getBuffer = bufferName => {
            return this.getBuffer(bufferName) || this.createBuffer({ name: bufferName });
        };

        // this will return a frame associated with the given buffer.
        let getFrame = (bufferName, point) => {
            let i = buffers.findIndex(([ buf ]) => buf == bufferName);
            if (i < 0) {
                // if we don't have an associated frame...
                if (frames.length > 0) {
                    // there's still one to spare
                    let fr = frames.pop();
                    fr.setBuffer(getBuffer(bufferName, point));
                    return fr;
                } else {
                    // otherwise, create new frame.
                    return this.createFrame({ buffer: getBuffer(bufferName), point: point });
                }
            } else {
                // found associated frame; remove it from the list.
                let fr = buffers[i][1];
                buffers.splice(i, 1);
                return fr;
            }
        };

        // 2. dig it again, this time creating widgets.
        let ops = [];
        let active = null;
        let main = (function dig(el){
            if (Array.isArray(el)) {
                let split = new Ymacs_SplitCont({ horiz: el[0] });
                let frame1 = dig(el[2]);
                let frame2 = dig(el[3]);
                ops.push(() => split.setSplit(frame1, frame2, el[1]));
                return split;
            } else {
                let frame = getFrame(el.buffer, el.point);
                if (el.active) active = frame;
                ops.push(() => {
                    let div = frame.getOverlaysContainer();
                    div.scrollTop = el.scroll * div.scrollHeight;
                });
                return frame;
            }
        })(config);

        // at this point, if we still have any frames left (e.g. the
        // editor had more frames than required by the new config),
        // remove/destroy them.
        frames.forEach(fr => {
            remove(this.frames, fr);
            fr.destroy();
        });

        let cont = this.getElement();
        cont.insertBefore(main.getElement(), cont.firstElementChild);

        // ops will contain operations to do after the elements are in
        // the DOM.
        ops.reverse().forEach(f => f());

        // done, reset active frame.
        this.setActiveFrame(active || this.frames[0]);
    }

    focus() {
        this.frames.at(-1).focus();
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
            frame.addClass("Ymacs_Frame-active");
        }
        this.__input_frame = frame;
        if (!nofocus)
            frame.focus();
    }

    getActiveFrame() {
        return this.frames.at(-1);
    }

    getActiveBuffer() {
        var frame = this.getActiveFrame();
        return frame ? frame.buffer : this.buffers.at(-1);
    }

    setColorTheme(themeId) {
        this.delClass(/Ymacs-Theme-[^\s]*/g);
        if (!(themeId instanceof Array))
            themeId = [ themeId ];
        themeId.forEach(themeId => {
            this.addClass("Ymacs-Theme-" + themeId);
        });
    }

    cursorBar() {
        this.delClass("Ymacs-cursor-block");
        this.addClass("Ymacs-cursor-bar");
    }
    cursorBlock() {
        this.delClass("Ymacs-cursor-bar");
        this.addClass("Ymacs-cursor-block");
    }
    toggleBarCursor() {
        if (this.hasClass("Ymacs-cursor-block")) {
            this.cursorBar();
        } else {
            this.cursorBlock();
        }
    }

    getFrameInDirection(dir) {
        let frame = this.getActiveFrame();
        let caret = frame.getCaretElement();
        let box = caret.getBoundingClientRect();
        var byx = [...this.frames].sort((a, b) => a.getBox().left - b.getBox().left);
        var byy = [...this.frames].sort((a, b) => a.getBox().top - b.getBox().top);
        return this["_get_frameInDir_" + dir](byx, byy, box, frame);
    }

    _get_frameInDir_left(byx, byy, pos, frame) {
        byx = byx.filter(f => {
            let p = f.getBox();
            return (f !== frame) && (p.left < pos.left) && (p.top - pos.height <= pos.top) && (p.top + p.height > pos.top);
        });
        return selectClosestFrameX(byx, pos);
    }

    _get_frameInDir_right(byx, byy, pos, frame) {
        byx.reverse();
        byx = byx.filter(f => {
            let p = f.getBox();
            return (f !== frame) && (p.left > pos.left) && (p.top - pos.height <= pos.top) && (p.top + p.height > pos.top);
        });
        return selectClosestFrameX(byx, pos);
    }

    _get_frameInDir_up(byx, byy, pos, frame) {
        byy = byy.filter(f => {
            let p = f.getBox();
            return (f !== frame) && (p.top < pos.top) && (p.left - pos.width <= pos.left) && (p.left + p.width > pos.left);
        });
        return selectClosestFrameY(byy, pos);
    }

    _get_frameInDir_down(byx, byy, pos, frame) {
        byy.reverse();
        byy = byy.filter(f => {
            let p = f.getBox();
            return (f !== frame) && (p.top > pos.top) && (p.left - pos.width <= pos.left) && (p.left + p.width > pos.left);
        });
        return selectClosestFrameY(byy, pos);
    }

    /* -----[ local storage ]----- */

    ls_get() {
        ensureLocalStorage();
        return JSON.parse(localStorage.getItem(this.o.ls_keyName) || "{}");
    }

    ls_set(src) {
        ensureLocalStorage();
        localStorage.setItem(this.o.ls_keyName, JSON.stringify(src));
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
                if (Object.hasOwn(info.dir, f)) {
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
            this.processKeyEvent(ev);
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

    processKeyEvent(ev) {
        var frame = this.__input_frame;
        var buffer = frame.buffer;

        if (this.__macro_recording) {
            this.__macro_recording.push(ev);
        }
        return buffer._handleKeyEvent(ev);
    }

    #timer_popupMessage = null;
    popupMessage({
        type = "info",
        text,
        isHtml = false,
        atCaret = false,
        timeout
    }) {
        let popup = this.__popup || (this.__popup = new Ymacs_Popup());
        let el = popup.getElement();
        popup.condClass(atCaret, "with-arrow");
        if (!atCaret) {
            el.style.removeProperty("left");
            el.style.removeProperty("top");
            el.style.removeProperty("bottom");
            el.style.removeProperty("right");
            el.style.removeProperty("transform");
        }
        if (!isHtml) {
            text = DOM.htmlEscape(text);
        }
        popup.setContent(text);
        if (atCaret) {
            this._popupAtCaret(popup.getElement());
        } else {
            this.add(popup);
        }
        clearTimeout(this.#timer_popupMessage);
        if (timeout) {
            this.#timer_popupMessage = setTimeout(this.clearPopupMessage.bind(this), timeout);
        }
    }

    clearPopupMessage() {
        if (this.__popup) {
            this.__popup.getElement().remove();
        }
    }

    requestFullScreen() {
        return this.getElement().requestFullscreen();
    }

    _popupAtCaret(el) {
        el.style.visibility = "hidden";
        DOM.delClass(el, /ppos-[a-z-]+/ig);
        this.add(el);
        let mybox = this.getElement().getBoundingClientRect();
        let caret = this.__input_frame.getCaretElement();
        let cbox = caret.getBoundingClientRect();
        let cbox_center = { x: (cbox.left + cbox.right) / 2,
                            y: (cbox.top + cbox.bottom) / 2 };
        let mybox_center = { x: (mybox.left + mybox.right) / 2,
                             y: (mybox.top + mybox.bottom) / 2 };
        if (cbox_center.y > mybox_center.y) {
            if (cbox_center.x < mybox_center.x) {
                DOM.addClass(el, "ppos-top-right");
                el.style.transform = `translate(0, calc(-100% - ${cbox.height / 2}px))`;
            } else {
                DOM.addClass(el, "ppos-top-left");
                el.style.transform = `translate(-100%, calc(-100% - ${cbox.height / 2}px))`;
            }
        } else {
            if (cbox_center.x < mybox_center.x) {
                DOM.addClass(el, "ppos-bot-right");
                el.style.transform = `translate(0, ${cbox.height / 2}px)`;
            } else {
                DOM.addClass(el, "ppos-bot-left");
                el.style.transform = `translate(-100%, ${cbox.height / 2}px)`;
            }
        }
        el.style.left = (cbox_center.x - mybox.left) + "px";
        el.style.top = (cbox_center.y - mybox.top) + "px";
        el.style.removeProperty("visibility");
    }

    jumpToRegister(reg) {
        let value = this.registers[reg];
        if (value == null) return false;
        if (value.frames) {
            this.setFrameConfig(value.frames);
            return true;
        }
    }
}
