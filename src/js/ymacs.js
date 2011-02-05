//> This file is part of Ymacs, an Emacs-like editor for the Web
//> http://www.ymacs.org/
//>
//> Copyright (c) 2009-2011, Mihai Bazon, Dynarch.com.  All rights reserved.
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

                // override in DlWidget
                _focusable : [ "focusable"  , true ]
        };

        D.FIXARGS = function(args) {
                if (!args.buffers)
                        args.buffers = [];
                if (!args.frames)
                        args.frames = [];
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
                        if (self.buffers.length > 1) {
                                if (self.getActiveBuffer() === buf)
                                        self.switchToNextBuffer();
                        } else {
                                // make a brand new buffer
                                self.switchToBuffer(self.createBuffer());
                        }
                        self.buffers.remove(buf);
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

        P.switchToNextBuffer = function(n) {
                var a = this.buffers;
                if (a.length > 1) {
                        var buf = a.shift();
                        a.push(buf);
                        this._do_switchToBuffer(a[0]);
                }
        };

        P.switchToPreviousBuffer = function(n) {
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
                return frame;
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

        P.setActiveFrame = function(frame, nofocus) {
                if (!frame.isMinibuffer) {
                        var old = this.getActiveFrame();
                        if (old) {
                                old.delClass("Ymacs_Frame-active");
                        }
                        this.frames.remove(frame);
                        this.frames.push(frame);
                }
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

        P.ls_getFileDirectory = function(name, create) {
                var store, dir = store = this.ls_get();
                name = name.replace(/^[~\x2f]+/, "").split(/\x2f+/);
                var path = [], other = [];
                while (name.length > 0) {
                        var part = name.shift();
                        if (dir.hasOwnProperty(part) && (typeof dir[part] != "string")) {
                                dir = dir[part];
                                path.push(part);
                        }
                        else {
                                other.push(part);
                        }
                };
                if (create) {
                        var n = create == "file" ? 1 : 0;
                        while (other.length > n) {
                                dir = dir[other.shift()] = {};
                        }
                        this.ls_set(store);
                }
                return { store: store, dir: dir, path: path, other: other };
        };

});
