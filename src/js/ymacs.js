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

DEFINE_CLASS("Ymacs", DlLayout, function(D, P, DOM){

    D.DEFAULT_EVENTS = [
        "onBufferSwitch",
        "onCreateBuffer",
        "onDeleteBuffer"
    ];

    D.DEFAULT_ARGS = {
        buffers : [ "buffers" , null ],
        frames  : [ "frames"  , null ],

        // default options
        cf_lineNumbers: [ "lineNumbers", false ],
        cf_frameStyle: [ "frameStyle", null ],
        cf_blinkCursor: [ "blinkCursor", true ],

        // override in DlWidget
        _focusable : [ "focusable"  , true ]
    };

    D.FIXARGS = function(args) {
        if (!args.buffers)
            args.buffers = [];
        if (!args.frames)
            args.frames = [];
        if (!args.cf_frameStyle)
            args.cf_frameStyle = {};
    };

    D.CONSTRUCT = function() {
        this.buffers.foreach(function(b){
            b.ymacs = this;
            this._addBufferListeners(b);
        }, this);

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

        this.packWidget(this.minibuffer_frame, { pos: "bottom" });
        this.packWidget(frame, { pos: "top", fill: "*" });

        // this.__activeFrameEvents = {
        //         // onPointChange: this._on_activeFramePointChange.$(this)
        // };

        this.setActiveFrame(frame);
        frame._redrawCaret();
    };

    P._addBufferListeners = function(buf) {
        var self = this;
        buf.addEventListener("onDestroy", function(){
            var fr = self.getActiveFrame();
            self.getBufferFrames(buf).foreach(function(f){
                if (f !== fr) {
                    self.deleteFrame(f);
                }
            });
            self.buffers.remove(buf);
            if (self.getActiveBuffer() === buf)
                self.nextHiddenBuffer(buf);
        });
    };

    P.pushToKillRing = function(text, prepend) {
        prepend ? this.killRing.unshift(text)
            : this.killRing.push(text);
    };

    P.killRingToMaster = function() {
        if (this.killRing.length && (this.killMasterOfRings.length == 0 ||
                                     this.killMasterOfRings.peek().join("") != this.killRing.join("")))
            this.killMasterOfRings.push(this.killRing);
        this.killRing = [];
    };

    P.killRingText = function() {
        return this.killRing.join("");
    };

    P.rotateKillRing = function(push) {
        if (push) {
            this.killMasterOfRings.push(this.killRing);
            this.killRing = this.killMasterOfRings.shift();
        } else {
            this.killMasterOfRings.unshift(this.killRing);
            this.killRing = this.killMasterOfRings.pop();
        }
    };

    P.getBuffer = function(buf) {
        if (!(buf instanceof Ymacs_Buffer)) {
            buf = this.buffers.grep_first(function(b){
                return b.name == buf;
            });
        }
        return buf;
    };

    P.killBuffer = function(buf) {
        buf = this.getBuffer(buf);
        this.callHooks("onDeleteBuffer", buf);
        buf.destroy();
    };

    P.renameBuffer = function(buf, name) {
        buf = this.getBuffer(buf);
        buf.name = name;
        buf.callHooks("onProgressChange");
    };

    P._do_switchToBuffer = function(buf) {
        this.getActiveFrame().setBuffer(buf);
        this.callHooks("onBufferSwitch", buf);
    };

    P.switchToBuffer = function(maybeName) {
        var buf = this.getBuffer(maybeName), a = this.buffers;
        if (!buf) {
            // create new buffer
            buf = this.createBuffer({ name: maybeName });
        }
        a.remove(buf);
        a.unshift(buf);
        this._do_switchToBuffer(buf);
        return buf;
    };

    P.nextHiddenBuffer = function(cur) {
        var a = this.buffers.grep(function(buf){
            if (buf === cur) return false;
            var hidden = true;
            buf.forAllFrames(function(){ hidden = false });
            return hidden;
        });
        if (a.length > 0) {
            var buf = a[0];
            this.buffers.remove(buf);
            this.buffers.push(buf);
            this._do_switchToBuffer(buf);
        } else {
            this.switchToBuffer("*scratch*");
        }
    };

    P.switchToNextBuffer = function() {
        var a = this.buffers;
        if (a.length > 1) {
            var buf = a.shift();
            a.push(buf);
            this._do_switchToBuffer(a[0]);
        }
    };

    P.switchToPreviousBuffer = function() {
        var a = this.buffers;
        if (a.length > 1) {
            var buf = a.pop();
            a.unshift(buf);
            this._do_switchToBuffer(buf);
        }
    };

    P.getNextBuffer = function(buf, n) {
        if (n == null) n = 1;
        var a = this.buffers;
        return a[a.rotateIndex(a.find(buf) + n)];
    };

    P.getPrevBuffer = function(buf, n) {
        if (n == null) n = 1;
        var a = this.buffers;
        return a[a.rotateIndex(a.find(buf) - n)];
    };

    P.getBufferFrames = function(buf) {
        buf = this.getBuffer(buf);
        return this.frames.grep(function(f){
            return f.buffer === buf;
        });
    };

    P.createBuffer = function(args) {
        if (!args) args = {};
        Object.merge(args, { ymacs: this });
        var buf = new Ymacs_Buffer(args);
        this._addBufferListeners(buf);
        if (!args.hidden)
            this.buffers.push(buf);
        this.callHooks("onCreateBuffer", buf);
        return buf;
    };

    P.createFrame = function(args) {
        if (!args) args = {};
        Object.merge(args, { ymacs: this });
        var frame = new Ymacs_Frame(args);
        if (!args.hidden)
            this.frames.unshift(frame);
        frame.addEventListener("onDestroy", function(frame) {
            this.frames.remove(frame);
        }.$(this, frame));
        frame.setStyle(this.cf_frameStyle);
        return frame;
    };

    P.setFrameStyle = function(style, reset) {
        style = this.cf_frameStyle = reset
            ? Object.makeCopy(style)
            : Object.merge(this.cf_frameStyle, style);
        [ this.minibuffer_frame ].concat(this.frames).foreach(function(frame){
            frame.setStyle(style);
            frame.setStyle("height", "");
        });
        this.minibuffer_frame.getOverlaysContainer().style.height = "";
        this.doLayout();
    };

    P.keepOnlyFrame = function(frame) {
        if (this.frames.length > 1) {
            var p = frame.parent;
            while (p.parent != this)
                p = p.parent;
            this.replaceWidget(p, frame);
            p.destroy();
            this.setActiveFrame(frame);
            this.doLayout();
        }
    };

    P.deleteFrame = function(frame) {
        if (this.frames.length > 1) {
            var p = frame.parent, other = p.children().grep_first(function(f){
                return f instanceof DlLayout || f instanceof Ymacs_Frame && f !== frame;
            });
            if (p._resizeBar) p._resizeBar._widget = other;
            p.parent.replaceWidget(p, other);
            p.destroy();
            try {
                DOM.walk(other.getElement(), function(el){
                    el = DlWidget.getFromElement(el);
                    if (el && el instanceof Ymacs_Frame)
                        throw el;
                });
            } catch(ex) {
                if (!(ex instanceof Ymacs_Frame))
                    throw ex;
                other = ex;
            }
            this.setActiveFrame(other);
            this.doLayout();
        }
    };

    P.focusOtherFrame = function() {
        this.setActiveFrame(this.frames[0]);
    };

    P.focus = function() {
        D.BASE.focus.apply(this, arguments);
        this.frames.peek().focus();
    };

    P.setInputFrame = function(frame) {
        this.__input_frame = frame;
    };

    P.setActiveFrame = function(frame, nofocus) {
        if (!frame.isMinibuffer) {
            var old = this.getActiveFrame();
            if (old) {
                old.delClass("Ymacs_Frame-active");
            }
            this.frames.remove(frame);
            this.frames.push(frame);
        }
        this.__input_frame = frame;
        if (!nofocus)
            frame.focus();
    };

    P.getActiveFrame = function() {
        return this.frames.peek();
    };

    P.getActiveBuffer = function() {
        var frame = this.getActiveFrame();
        return frame ? frame.buffer : this.buffers.peek();
    };

    P.setColorTheme = function(themeId) {
        this.delClass(/Ymacs-Theme-[^\s]*/g);
        if (!(themeId instanceof Array))
            themeId = [ themeId ];
        themeId.foreach(function(themeId){
            this.addClass("Ymacs-Theme-" + themeId);
        }, this);
    };

    P.getFrameInDirection = function(dir, pos, frame) {
        if (!frame)
            frame = this.getActiveFrame();
        var caret = frame.getCaretElement();
        if (!pos)
            pos = DOM.getPos(caret);
        if (!pos.sz)
            pos.sz = DOM.getOuterSize(caret);
        var byx = this.frames.mergeSort(function(a, b){ return a.getPos().x - b.getPos().x });
        var byy = this.frames.mergeSort(function(a, b){ return a.getPos().y - b.getPos().y });
        return this["_get_frameInDir_" + dir](byx, byy, pos, frame);
    };

    function selectClosestFrameX(byx, pos) {
        if (byx.length > 0) {
            var x = byx.peek().getPos().x, a = [ byx.pop() ];
            while (byx.length > 0 && byx.peek().getPos().x == x)
                a.push(byx.pop());
            return a.minElement(function(f){
                return Math.abs(pos.y - f.getPos().y - f.getSize().y/2);
            });
        }
    };

    function selectClosestFrameY(byy, pos) {
        if (byy.length > 0) {
            var y = byy.peek().getPos().y, a = [ byy.pop() ];
            while (byy.length > 0 && byy.peek().getPos().y == y)
                a.push(byy.pop());
            return a.minElement(function(f){
                return Math.abs(pos.x - f.getPos().x - f.getSize().x/2);
            });
        }
    };

    P._get_frameInDir_left = function(byx, byy, pos, frame) {
        byx = byx.grep(function(f){
            var p = f.getPos(), s = f.getSize();
            return (f !== frame) && (p.x < pos.x) && (p.y - pos.sz.y <= pos.y) && (p.y + s.y > pos.y);
        });
        return selectClosestFrameX(byx, pos);
    };

    P._get_frameInDir_right = function(byx, byy, pos, frame) {
        byx.reverse();
        byx = byx.grep(function(f){
            var p = f.getPos(), s = f.getSize();
            return (f !== frame) && (p.x > pos.x) && (p.y - pos.sz.y <= pos.y) && (p.y + s.y > pos.y);
        });
        return selectClosestFrameX(byx, pos);
    };

    P._get_frameInDir_up = function(byx, byy, pos, frame) {
        byy = byy.grep(function(f){
            var p = f.getPos(), s = f.getSize();
            return (f !== frame) && (p.y < pos.y) && (p.x - pos.sz.x <= pos.x) && (p.x + s.x > pos.x);
        });
        return selectClosestFrameY(byy, pos);
    };

    P._get_frameInDir_down = function(byx, byy, pos, frame) {
        byy.reverse();
        byy = byy.grep(function(f){
            var p = f.getPos(), s = f.getSize();
            return (f !== frame) && (p.y > pos.y) && (p.x - pos.sz.x <= pos.x) && (p.x + s.x > pos.x);
        });
        return selectClosestFrameY(byy, pos);
    };

    /* -----[ local storage ]----- */

    function ensureLocalStorage() {
        if (!(window.localStorage && window.localStorage.getItem))
            throw new Ymacs_Exception("Local storage facility not available in this browser");
    };

    P.ls_get = function() {
        ensureLocalStorage();
        return DlJSON.decode(localStorage.getItem(".ymacs") || "{}", true);
    };

    P.ls_set = function(src) {
        ensureLocalStorage();
        localStorage.setItem(".ymacs", DlJSON.encode(src));
    };

    P.ls_getFileContents = function(name, nothrow) {
        var info = this.ls_getFileDirectory(name), other = info.other, code;
        if (other.length == 1) {
            code = info.dir[other[0]];
        }
        if (code == null && !nothrow) {
            throw new Ymacs_Exception("File not found");
        }
        return code;
    };

    P.ls_setFileContents = function(name, content) {
        var files = this.ls_getFileDirectory(name, "file");
        files.dir[files.other[0]] = content;
        this.ls_set(files.store);
    };

    P.ls_getFileDirectory = function(name, create) {
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
    };

    P.ls_deleteFile = function(name) {
        var info = this.ls_getFileDirectory(name);
        delete info.dir[info.other.join("/")];
        this.ls_set(info.store);
    };

    /* -----[ filesystem operations ]----- */

    P.fs_normalizePath = function(path) {
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
    };

    P.fs_fileType = function(name, cont) {
        try {
            this.ls_getFileContents(name);
            cont(true);
        } catch(ex) {
            cont(null);
        }
    };

    P.fs_getFileContents = function(name, nothrow, cont) {
        var code = this.ls_getFileContents(name, nothrow);
        cont(code, code); // second parameter is file stamp, on a real fs it should be last modification time
    };

    P.fs_setFileContents = function(name, content, stamp, cont) {
        if (stamp && (this.ls_getFileContents(name, true) || "") != stamp) {
            cont(null); // did not change file because stamp is wrong
        } else {
            this.ls_setFileContents(name, content);
            cont(content);
        }
    };

    P.fs_getDirectory = function(dirname, cont) {
        var info = this.ls_getFileDirectory(dirname, false);
        dirname = info.path.join("/"); // normalized
        if (info) {
            var files = {};
            for (var f in info.dir) {
                if (Object.HOP(info.dir, f)) {
                    files[f] = { name : f,
                                 path : dirname + "/" + f,
                                 type : (typeof info.dir[f] == "string"
                                         ? "regular"
                                         : "directory")};
                }
            }
            cont(files);
        } else {
            cont(null);
        }
    };

    P.fs_deleteFile = function(name, cont) {
        this.ls_deleteFile(name);
        cont();
    };

    P.fs_remapDir = function(dir, cont) {
        cont(dir);
    };

    P.isRunningMacro = function() {
        return !!this.__running_macro;
    };

    P.isRecordingMacro = function() {
        return !!this.__macro_recording;
    };

    P.indicateError = function() {
        this.__error_thrown = true;
    };

    P.startMacro = function(do_append) {
        if (this.isRecordingMacro())
            return false;
        if (do_append) {
            this.__macro_recording = this.__macro_finished || [];
            this.__macro_finished = null;
        } else
            this.__macro_recording = [];
        return true;
    };

    P.stopMacro = function() {
        if (this.__macro_recording) {
            this.__macro_finished = this.__macro_recording;
            this.__macro_recording = null;
        }
    };

    P.getLastMacro = function() {
        return this.__macro_finished;
    };

    P.stepMacro = function() {
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
    };

    P.runMacro = function(times, macro) {
        if (this.isRecordingMacro())
            return false;
        this.__error_thrown = false;
        this.__running_macro = macro;
        this.__macro_step = 0;
        this.__macro_times = times;
        var self = this;
        setTimeout(function() { self.stepMacro(); }, 0);
        return true;
    };

    P.processKeyEvent = function(ev,press) {
        var frame = this.__input_frame;
        var buffer = frame.buffer;

        ev.wasKeypress = press;
        if (!ev._keyCode)
          ev._keyCode = ev.keyCode;

        if (press) {
            if (!is_gecko)
                ev.keyCode = 0;
            if (this.__macro_recording) {
                this.__macro_recording.push(ev);
            }
            return buffer._handleKeyEvent(ev);
        } else {
            if (!is_gecko) {
                var ki = window.KEYBOARD_INSANITY, code = ev.keyCode || ev._keyCode;
                if (code == 0 || code in ki.modifiers)
                    return false;
                if ((code in ki.letters || code in ki.digits || code in ki.symbols) && !(ev.ctrlKey || ev.altKey)) {
                    return false; // to be handled by the upcoming keypress event
                }
                ev.charCode = ki.getCharCode(code, ev.shiftKey);
                if (ev.charCode)
                    ev.keyCode = 0;
                if (this.__macro_recording) {
                    this.__macro_recording.push(ev);
                }
                return buffer._handleKeyEvent(ev);
            }
        }
    };
});
