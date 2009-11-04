DEFINE_CLASS("Ymacs", DlLayout, function(D, P){

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
                this.minibuffer = this.createBuffer({ hidden: true });
                this.minibuffer_frame = this.createFrame({
                        buffer: this.minibuffer,
                        hidden: true,
                        highlightCurrentLine: false,
                        className: "Ymacs_Minibuffer"
                });

                /* -----[ mode line ]----- */
                this.modeline = new DlContainer({ className: "Ymacs_Modeline" });
                this.modeline.setContent("--Ymacs--");
                /* -----[ main content ]----- */
                if (this.buffers.length == 0)
                        this.createBuffer();
                var frameLayout = this.frameLayout = new DlLayout({});

                // var frame = this.createFrame({ buffer: this.buffers[0] });
                // frameLayout.packWidget(frame, { pos: "top", fill: "50%" });
                // frameLayout.packWidget(new DlResizeBar({ widget: frame, horiz: true }), { pos: "top" });

                var frame = this.createFrame({ buffer: this.buffers[0] });
                frameLayout.packWidget(frame, { pos: "top", fill: "*" });

                this.packWidget(this.minibuffer_frame, { pos: "bottom" });
                this.packWidget(this.modeline, { pos: "bottom" });
                this.packWidget(frameLayout, { pos: "top", fill: "*" });

                this.__activeFrameEvents = {
                        onPointChange: this._on_activeFramePointChange.$(this)
                };

                this.setActiveFrame(frame);
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

        P.createBuffer = function(args) {
                if (!args) args = {};
                Object.merge(args, { ymacs: this });
                var buf = new Ymacs_Buffer(args);
                if (!args.hidden)
                        this.buffers.push(buf);
                return buf;
        };

        P.createFrame = function(args) {
                if (!args) args = {};
                Object.merge(args, { ymacs: this });
                var frame = new Ymacs_Frame(args);
                if (!args.hidden)
                        this.frames.push(frame);
                frame.addEventListener("onDestroy", function(frame) {
                        this.frames.remove(frame);
                }.$(this, frame));
                return frame;
        };

        P.focus = function() {
                D.BASE.focus.apply(this, arguments);
                this.frames.peek().focus();
        };

        P.setActiveFrame = function(frame) {
                var old = this.getActiveFrame();
                if (old)
                        old.removeEventListener(this.__activeFrameEvents);
                this.frames.remove(frame);
                this.frames.push(frame);
                this.updateModelineWithTimer();
                frame.addEventListener(this.__activeFrameEvents);
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
                if (this.modeline) {
                        var buffer = this.getActiveBuffer();
                        var rc = buffer._rowcol;
                        var ml = String.buffer("-U:---  ", buffer.name, " (", rc.row + 1, ",", rc.col, ") ");
                        var pr = [];
                        for (var i in this.progress) {
                                pr.push(i + ": " + this.progress[i]);
                        }
                        if (pr.length > 0) {
                                ml("[", pr.join(", "), "]");
                        }
                        this.modeline.setContent(ml.get().htmlEscape());
                }
        };

        /* -----[ listeners ]----- */

        P._on_activeFramePointChange = function() {
                this.updateModeline();
        };

});
