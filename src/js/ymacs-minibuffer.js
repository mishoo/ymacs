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

// @require ymacs-buffer.js
// @require ymacs-keymap.js

Ymacs_Buffer.newMode("minibuffer_mode", function(){
    var marker = this.createMarker(0, true);
    var changed_vars = this.setq({
        minibuffer_end_marker: marker
    });
    var keymap = Ymacs_Keymap_Minibuffer();
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
        var self = this;        // minibuffer
        if ($menu)
            $menu.destroy();
        $menu = new DlVMenu({});
        list.foreach(function(item, index){
            var data = item;
            if (typeof item != "string") {
                data = item.completion;
                item = item.label;
            }
            new DlMenuItem({ parent: $menu, label: item.htmlEscape(), data: data, name: index })
                .addEventListener("onMouseEnter", function(){
                    if ($item != index) {
                        if ($item != null) {
                            $menu.children($item).callHooks("onMouseLeave");
                        }
                        $item = index;
                    }
                });
        });
        $menu.addEventListener({
            onSelect: function(index, item){
                $item = index;
                handle_enter.call(self);
            }
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
                self.popKeymap(KEYMAP_POPUP_ACTIVE);
                // $menu.destroy();
                $item = null;
                $menu = null;
            },
            isContext: true
        });
        $popupActive = true;
        self.pushKeymap(KEYMAP_POPUP_ACTIVE);

        handle_arrow_down.call(self); // autoselect the first one anyway
    };

    function read_with_continuation(completions, cont, validate) {
        this.whenMinibuffer(function(mb){
            var changed_vars = mb.setq({
                completion_list: completions,
                minibuffer_validation: function(what, cont2){
                    if (what == null)
                        what = mb.cmd("minibuffer_contents");
                    if (validate)
                        validate.call(this, mb, what, cont2);
                    else
                        cont2(true); // accept anything by default
                }.$(this),
                minibuffer_continuation: function(what){
                    mb.setq(changed_vars);
                    if (cont)
                        cont.call(this, what);
                }.$(this)
            });
        });
    };

    function filename_completion(mb, str, re, cont) {

        var self = this;
        var lastslash = str.lastIndexOf("/");
        var dir = str.slice(0, lastslash+1);
        var partial = str.slice(lastslash+1);
        self.ymacs.fs_getDirectory(dir, function (files) {

            function add_trailing_slash_to_dir(name) {
                return (files[name].type == "directory") ? name+"/" : name;
            }

            if (!files) {
                mb.signalError("Not found");
                cont(null);
            } else {
                var completions = [];
                for (var f in files) {
                    if (f.indexOf(partial) == 0) {
                        completions.push(add_trailing_slash_to_dir(f));
                    }
                }
                if (completions.length == 0) {
                    cont([]);
                } else {
                    var prefix = completions.common_prefix();
                    if (prefix != partial) {
                        mb.cmd("minibuffer_replace_input", dir+prefix);
                        cont(null);
                    } else if (completions.length == 1) {
                        cont([str]);
                    } else {
                        completions = completions.map(function(name){
                            return {label: name, completion: dir+name};
                        });
                        popupCompletionMenu.call(mb, self.getMinibufferFrame(), completions);
                        cont(null);
                    }
                }
            }
        });
    };

    Ymacs_Buffer.newCommands({

        minibuffer_prompt: function(prompt, nofocus) {
            this.whenMinibuffer(function(mb){
                var f = this.getMinibufferFrame();
                this.ymacs.setInputFrame(f);
                mb.setCode("");
                mb.cmd("prevent_undo", function(){
                    mb.cmd("insert", prompt);
                });
                mb.getq("minibuffer_end_marker").setPosition(mb.point());
                mb._textProperties.addLineProps(0, 0, mb.point(), "css", "minibuffer-prompt");
                f._redrawCaret(true);
                if (!nofocus)
                    f.focus();
            });
        },

        minibuffer_yn: function(prompt, cont) {
            this.cmd("minibuffer_prompt", prompt + " (yes or no) ");
            this.cmd("minibuffer_read_yn", function (text) {
                cont(text == "yes");
            });
        },

        minibuffer_read_yn: function(cont) {
            read_with_continuation.call(this, [ "yes", "no" ], cont, function(mb, text, cont2){
                if (text == "yes" || text == "no")
                    cont2(true);
                else
                    mb.signalError("Please enter yes or no");
            });
        },

        minibuffer_read_number: function(cont) {
            read_with_continuation.call(this, null, cont, function(mb, text, cont2){
                var n = parseInt(text, 10);
                if (isNaN(n))
                    mb.signalError("Please enter a number");
                cont2(!isNaN(n));
            });
        },

        minibuffer_read_command: function(cont) {
            var commandNames = Array.hashKeys(this.COMMANDS).grep(function(cmd){
                return this.COMMANDS[cmd].ymacsInteractive;
            }, this).sort();
            read_with_continuation.call(this, commandNames, cont, function(mb, name, cont2){
                var cmd = this.COMMANDS[name],
                ret = cmd && cmd.ymacsInteractive;
                if (!ret) {
                    mb.signalError("No such command: " + name);
                }
                cont2(ret);
            });
        },

        minibuffer_read_function: function(cont) {
            var commandNames = Array.hashKeys(this.COMMANDS).sort();
            read_with_continuation.call(this, commandNames, cont, function(mb, name, cont2){
                var cmd = this.COMMANDS[name],
                ret = !!cmd;
                if (!ret)
                    mb.signalError("No such function: " + name);
                cont2(ret);
            });
        },

        minibuffer_read_buffer: function(cont) {
            this.whenYmacs(function(ymacs){
                var bufferNames = ymacs.buffers.map("name");
                bufferNames.push(bufferNames.shift());
                read_with_continuation.call(this, bufferNames, cont);
                //handle_tab.call(this);
            });
        },

        minibuffer_read_string: function(completions, cont) {
            read_with_continuation.call(this, completions, cont);
        },

        minibuffer_read_variable: function(cont) {
            var tmp = this.globalVariables;
            Object.merge(tmp, this.variables);
            var completions = Array.hashKeys(tmp).grep(function(name){
                return !/^\*/.test(name);
            }).sort();
            read_with_continuation.call(this, completions, cont
                                        // XXX: seems like a good idea, but it doesn't work
                                        // XXX: need to refactor the signalInfo stuff.  It doesn't show up
                                        //      currently because the buffer frame is not active, or something...
                                        // , function(mb, name){
                                        //         var val = this.getq(name);
                                        //         mb.signalInfo("Current value of " + name + ": " + val);
                                        //         return true;
                                        // }
                                       );
        },

        minibuffer_read_existing_file: function(cont) {
            var self = this;
            self.cmd("minibuffer_replace_input_by_current_dir", function () {
                read_with_continuation.call(self, filename_completion, cont, function(mb, name, cont2){
                    self.ymacs.fs_fileType(name, function (type) {
                        if (type == null) {
                            mb.signalError("No such file: " + name);
                            cont2(false);
                        } else {
                            cont2(true);
                        }
                    });
                });
            });
        },

        minibuffer_read_file: function(cont) {
            var self = this;
            self.cmd("minibuffer_replace_input_by_current_dir", function () {
                read_with_continuation.call(self, filename_completion, cont);
            });
        },

        minibuffer_read_file_or_directory: function(cont) {
            var self = this;
            self.cmd("minibuffer_replace_input_by_current_dir", function () {
                read_with_continuation.call(self, filename_completion, cont);
            });
        },

        minibuffer_read_directory: function(cont) {
            var self = this;
            self.cmd("minibuffer_replace_input_by_current_dir", function () {
                read_with_continuation.call(self, filename_completion, cont);
            });
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

        minibuffer_replace_input_by_current_dir: function (cont) {
            this.whenYmacs(function(ymacs){
                var self = this;
                var name = ymacs.getActiveBuffer().name;
                var dir = name.slice(0, name.lastIndexOf("/")+1);
                ymacs.fs_remapDir(dir, function (dir) {
                    self.cmd("minibuffer_replace_input", dir);
                    cont();
                });
            });
        },

        minibuffer_complete: function() {
            var self = this;
            self.whenMinibuffer(function(mb){

                function complete(a) {
                    if (!a || a.length == 0) {
                        mb.signalError("No completions");
                    }
                    else {
                        var prefix = a.common_prefix();
                        if (prefix != str) {
                            mb.cmd("minibuffer_replace_input", prefix);
                        }
                        else if (a.length == 1) {
                            mb.signalError("Sole completion");
                        }
                        else {
                            popupCompletionMenu.call(mb, self.getMinibufferFrame(), a);
                        }
                    }
                }

                var a = mb.getq("completion_list"),
                str = mb.cmd("minibuffer_contents"),
                re = str.replace(/([\[\]\(\)\{\}\.\*\+\?\|\\])/g, "\\$1").replace(/([_-])/g, "[^_-]*[_-]");
                re = new RegExp("^" + re, "i");
                if (a instanceof Function) {
                    a.call(self, mb, str, re, function (a) {
                        if (a)
                            complete(a);
                    });
                }
                else if (a && a.length > 0) {
                    a = a.grep(function(cmd){
                        return re.test(cmd);
                    });
                    complete(a);
                }
                else
                    complete(a);
            });
        },

        minibuffer_complete_and_exit: function() {
            var self = this;
            self.whenMinibuffer(function(mb){
                mb.getq("minibuffer_validation").call(mb, null, function (valid) {
                    if (valid)
                        mb.cmd("minibuffer_keyboard_quit", self.getq("minibuffer_continuation"));
                });
            });
        },

        minibuffer_keyboard_quit: function(cont) {
            this.whenMinibuffer(function(mb){
                var text = this.cmd("minibuffer_contents");
                mb.setCode("");
                this.ymacs.setInputFrame(this.ymacs.getActiveFrame());
                this.ymacs.getActiveFrame().focus();
                (function(text){
                    if (cont)
                        cont.call(this, text);
                    this.getPrefixArg();
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
        } else {
            this.cmd("minibuffer_complete_and_exit");
        }
    };

    function handle_tab() {
        if (!$popupActive)
            this.cmd("minibuffer_complete");
        else
            handle_arrow_down.call(this);
    };

    function handle_s_tab() {
        handle_arrow_up.call(this);
    };

    function handle_home() {
        this.cmd("goto_char", this.getq("minibuffer_end_marker"));
    };

    function handle_home_mark() {
        this.ensureTransientMark();
        this.cmd("goto_char", this.getq("minibuffer_end_marker"));
        this.ensureTransientMark();
    };

    var DEFAULT_KEYS = {
        "TAB"                                : handle_tab,
        "ENTER"                              : handle_enter,
        "HOME && C-a"                        : handle_home,
        "S-HOME && S-C-a"                    : Ymacs_Interactive("^", handle_home_mark)
    };

    DEFINE_SINGLETON("Ymacs_Keymap_Minibuffer", Ymacs_Keymap, function(D, P){
        D.KEYS = Object.merge({
            "C-g && ESCAPE" : "minibuffer_keyboard_quit"
        }, DEFAULT_KEYS);
    });

    var KEYMAP_POPUP_ACTIVE = DEFINE_CLASS(null, Ymacs_Keymap, function(D, P){
        D.KEYS = Object.merge({
            "S-TAB"                                   : handle_s_tab,
            "ARROW_DOWN && ARROW_RIGHT && C-n && C-f" : handle_arrow_down,
            "ARROW_UP && ARROW_LEFT && C-p && C-b"    : handle_arrow_up,
            "ESCAPE"                                  : function() {
                DlPopup.clearAllPopups();
            }
        }, DEFAULT_KEYS);
        P.defaultHandler = [ function() {
            DlPopup.clearAllPopups();
            return false; // say it's not handled though
        } ];
    });

    KEYMAP_POPUP_ACTIVE = new KEYMAP_POPUP_ACTIVE();

})();

DEFINE_CLASS("Ymacs_Completion_Popup", DlCompletionPopup);
