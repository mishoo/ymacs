// This file is part of Ymacs, an extensible source code editor
// (c) Mihai Bazon 2009 <mihai.bazon@gmail.com>
// Distributed under a BSD-style license.
// http://www.ymacs.org/

DEFINE_CLASS("Ymacs", DlLayout, function(D, P){

        D.DEFAULT_EVENTS = [ "onBufferSwitch" ];

        D.DEFAULT_ARGS = {
                buffers : [ "buffers" , null ],
                frames  : [ "frames"  , null ],

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
                }, this);

                /* -----[ variables ]----- */
                this.killRing = [];
                this.killMasterOfRings = [];
                this.progress = {};

                /* -----[ closures ]----- */
                this.updateModelineWithTimer = this.updateModeline.clearingTimeout(0, this);

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

                /* -----[ mode line ]----- */
                this.modeline = new DlContainer({ className: "Ymacs_Modeline" });
                this.modeline.setContent("--Ymacs--");

                /* -----[ main content ]----- */
                if (this.buffers.length == 0)
                        this.createBuffer();

                var topCont = this.topCont = new DlContainer({});
                var frame = this.createFrame({ parent: topCont, buffer: this.buffers[0] });

                this.packWidget(this.minibuffer_frame, { pos: "bottom" });
                this.packWidget(this.modeline, { pos: "bottom" });
                this.packWidget(topCont, { pos: "top", fill: "*" });

                this.__activeFrameEvents = {
                        onPointChange: this._on_activeFramePointChange.$(this)
                };

                this.setActiveFrame(frame);
                frame._redrawCaret();
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
                if (this.buffers.length > 1) {
                        if (this.getActiveBuffer() === buf)
                                this.switchToNextBuffer();
                } else {
                        // make a brand new buffer
                        this.switchToBuffer(this.createBuffer());
                }
                this.buffers.remove(buf);
                buf.destroy();
        };

        P.renameBuffer = function(buf, name) {
                buf = this.getBuffer(buf);
                buf.name = name;
                this.updateModelineWithTimer();
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

        P.createBuffer = function(args) {
                if (!args) args = {};
                Object.merge(args, { ymacs: this });
                var buf = new Ymacs_Buffer(args);
                if (!args.hidden)
                        this.buffers.push(buf);
                //
                // XXX: although this seems the right way to do it,
                //      instead of doing it in killBuffer, for some
                //      reason we never get this event.  Should
                //      investigate.
                //
                // buf.addEventListener("onDestroy", function(buf){
                //         console.log("got here, %s, %s", this.getActiveBuffer().name, buf.name);
                //         if (this.getActiveBuffer() === buf)
                //                 this.switchToPreviousBuffer();
                //         this.buffers.remove(buf);
                // }.$(this, buf));
                //
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
                        frame.parent.removeWidget(frame);
                        this.frames.remove(frame);
                        this.topCont.destroyChildWidgets();
                        this.topCont.appendWidget(frame);
                        this.topCont.__doLayout();
                        this.setActiveFrame(frame);
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
                var old = this.getActiveFrame();
                if (old)
                        old.removeEventListener(this.__activeFrameEvents);
                if (!frame.isMinibuffer) {
                        this.frames.remove(frame);
                        this.frames.push(frame);
                } else {
                        this.frames.unshift(this.frames.pop());
                }
                this.updateModelineWithTimer();
                frame.addEventListener(this.__activeFrameEvents);
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

        P.updateProgress = function(name, val) {
                if (val == null)
                        delete this.progress[name];
                else
                        this.progress[name] = val;
                this.updateModelineWithTimer();
        };

        P.updateModeline = function() {
                var buffer = this.getActiveBuffer();
                if (this.modeline && buffer !== this.minibuffer) {
                        var rc = buffer._rowcol;
                        var ml = String.buffer("-- <b>", buffer.name.htmlEscape(), "</b> (", rc.row + 1, ",", rc.col, ") ");
                        var pr = [];
                        for (var i in this.progress) {
                                pr.push(i + ": " + this.progress[i]);
                        }
                        if (pr.length > 0) {
                                ml("[", pr.join(", "), "]");
                        }
                        this.modeline.setContent(ml.get());
                }
        };

        P.setColorTheme = function(themeId) {
                this.delClass(/Ymacs-Theme-[^\s]*/g);
                this.addClass("Ymacs-Theme-" + themeId);
        };

        /* -----[ listeners ]----- */

        P._on_activeFramePointChange = function() {
                this.updateModelineWithTimer();
        };

});
