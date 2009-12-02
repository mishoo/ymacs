// This file is part of Ymacs, an extensible source code editor
// (c) Mihai Bazon 2009 <mihai.bazon@gmail.com>
// Distributed under a BSD-style license.
// http://www.ymacs.org/

// @require ymacs-buffer.js
// @require ymacs-keymap.js

Ymacs_Buffer.newMode("minibuffer_mode", function(){
        var marker = this.createMarker(0, true);
        var changed_vars = this.setq({
                minibuffer_end_marker: marker
        });
        var keymap = new Ymacs_Keymap_Minibuffer({ buffer: this });
        this.pushKeymap(keymap);
        return function() {
                this.setq(changed_vars);
                marker.destroy();
                this.popKeymap(keymap);
        };
});

(function(){

        var $popupActive = false;
        var $menu = null, $item = null;
        function popupCompletionMenu(frame, list) {
                if ($menu)
                        $menu.destroy();
                $menu = new DlVMenu({});
                list.foreach(function(item){
                        new DlMenuItem({ parent: $menu, label: item.htmlEscape(), data: item });
                });
                var popup = Ymacs_Completion_Popup.get();
                popup.popup({
                        timeout: 0,
                        content: $menu,
                        align: {
		                prefer: "Tr",
		                fallX1: "_r",
		                fallX2: "_L",
		                fallY1: "B_",
		                fallY2: "T_"
	                },
                        anchor: frame.getCaretElement(),
                        widget: frame,
                        onHide: function() {
                                $popupActive = false;
                                // $menu.destroy();
                                $item = null;
                                $menu = null;
                        },
                        isContext: true
                });
                $popupActive = true;
        };

        function read_with_continuation(completions, cont) {
                this.whenMinibuffer(function(mb){
                        var changed_vars = mb.setq({
                                completion_list: completions,
                                minibuffer_continuation: function(what){
                                        mb.setq(changed_vars);
                                        if (cont)
                                                cont.call(this, what);
                                }.$(this)
                        });
                });
        };

        Ymacs_Buffer.newCommands({

                minibuffer_prompt: function(prompt, nofocus) {
                        this.whenMinibuffer(function(mb){
                                var f = this.getMinibufferFrame();
                                mb.setCode("");
                                mb.cmd("prevent_undo", function(){
                                        mb.cmd("insert", prompt);
                                });
                                mb.getq("minibuffer_end_marker").setPosition(mb.point());
                                f._redrawCaret(true);
                                if (!nofocus)
                                        f.focus();
                        });
                },

                minibuffer_read_command: function(cont) {
                        var commandNames = Array.hashKeys(this.COMMANDS).sort();
                        read_with_continuation.call(this, commandNames, cont);
                },

                minibuffer_read_buffer: function(cont) {
                        this.whenYmacs(function(ymacs){
                                var bufferNames = ymacs.buffers.map("name");
                                bufferNames.push(bufferNames.shift());
                                read_with_continuation.call(this, bufferNames, cont);
                        });
                },

                minibuffer_read_string: function(completions, cont) {
                        read_with_continuation.call(this, completions, cont);
                },

                minibuffer_prompt_end: function() {
                        return this.whenMinibuffer(function(mb){
                                return mb.getq("minibuffer_end_marker").getPosition();
                        });
                },

                minibuffer_contents: function() {
                        return this.whenMinibuffer(function(mb){
                                return mb._bufferSubstring(mb.getq("minibuffer_end_marker"));
                        });
                },

                minibuffer_replace_input: function(value) {
                        this.whenMinibuffer(function(mb){
                                mb._replaceText(mb.getq("minibuffer_end_marker"), mb.getCodeSize(), value);
                                this.getMinibufferFrame()._redrawCaret(true);
                        });
                },

                minibuffer_complete: function() {
                        this.whenMinibuffer(function(mb){
                                var a = mb.getq("completion_list"),
                                    str = mb.cmd("minibuffer_contents");
                                if (a && a.length > 0) {
                                        a = a.grep(function(cmd){
                                                return cmd.indexOf(str) == 0;
                                        });
                                }
                                if (!a || a.length == 0) {
                                        mb.signalError("No completions");
                                }
                                else {
                                        var prefix = a.common_prefix();
                                        if (prefix != str) {
                                                mb.cmd("minibuffer_replace_input", prefix);
                                        }
                                        else if (a.length == 1) {
                                                this.signalError("Sole completion");
                                        }
                                        else {
                                                popupCompletionMenu(this.getMinibufferFrame(), a);
                                        }
                                }
                        });
                },

                minibuffer_complete_and_exit: function() {
                        // this.cmd("minibuffer_complete");
                        // DlPopup.clearAllPopups();
                        this.whenMinibuffer(function(mb){
                                mb.cmd("minibuffer_keyboard_quit", this.getq("minibuffer_continuation"));
                        });
                },

                minibuffer_keyboard_quit: function(cont) {
                        this.whenMinibuffer(function(mb){
                                var text = this.cmd("minibuffer_contents");
                                mb.setCode("");
                                this.cmd("other_frame");
                                (function(text){
                                        if (cont)
                                                cont.call(this, text);
                                }).delayed(1, this, text);
                        });
                        DlPopup.clearAllPopups();
                }

        });

        function handle_completion(how) {
                var old_item = $item, w;
                switch (how) {
                    case "next":
                        if ($item == null)
                                $item = -1;
                        $item = $menu.children().rotateIndex(++$item);
                        break;
                    case "prev":
                        if ($item == null)
                                $item = 0;
                        $item = $menu.children().rotateIndex(--$item);
                        break;
                }
                if (old_item != null) {
                        w = $menu.children(old_item);
			w.callHooks("onMouseLeave");
                }
                old_item = $item;
                w = $menu.children($item);
		w.callHooks("onMouseEnter");
        };

        function handle_arrow_down() {
                if ($popupActive) {
                        return handle_completion.call(this, "next");
                }
        };

        function handle_arrow_up() {
                if ($popupActive) {
                        return handle_completion.call(this, "prev");
                }
        };

        function handle_enter() {
                if ($popupActive) {
                        if ($item != null) {
                                this.cmd("minibuffer_replace_input", $menu.children()[$item].userData);
                                DlPopup.clearAllPopups();
                        } else {
                                this.signalError("Select something...");
                        }
                }
                this.cmd("minibuffer_complete_and_exit");
        };

        function handle_tab() {
                if ($popupActive) {
                        handle_arrow_down.call(this);
                } else {
                        this.cmd("minibuffer_complete");
                }
        };

        function handle_s_tab() {
                handle_arrow_up.call(this);
        };

        function handle_escape() {
                if ($popupActive) {
                        DlPopup.clearAllPopups();
                } else {
                        this.cmd("minibuffer_keyboard_quit");
                }
        };

        DEFINE_CLASS("Ymacs_Keymap_Minibuffer", Ymacs_Keymap, function(D, P){

                D.KEYS = {
                        "C-g"         : "minibuffer_keyboard_quit",
                        "TAB"         : handle_tab,
                        "S-TAB"       : handle_s_tab,
                        "ARROW_DOWN"  : handle_arrow_down,
                        "ARROW_UP"    : handle_arrow_up,
                        "ENTER"       : handle_enter,
                        "ESCAPE"      : handle_escape
                };

                D.CONSTRUCT = function() {
                        this.defaultHandler = function() {
                                DlPopup.clearAllPopups();
                                return false; // say it's not handled though
                        };
                        this.defineKeys(D.KEYS);
                };

        });

})();

DEFINE_CLASS("Ymacs_Completion_Popup", DlCompletionPopup);
