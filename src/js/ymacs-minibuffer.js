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

import "./ymacs-buffer.js";
import { Ymacs_Keymap } from "./ymacs-keymap.js";
import { DOM, common_prefix } from "./ymacs-utils.js";
import { Ymacs_Popup } from "./ymacs-popup.js";

Ymacs_Buffer.newMode("minibuffer_mode", function(){
    var marker = this.createMarker(0, true);
    var changed_vars = this.setq({
        minibuffer_end_marker: marker
    });
    var keymap = Ymacs_Keymap.get("minibuffer");
    this.pushKeymap(keymap);
    return function() {
        this.setq(changed_vars);
        marker.destroy();
        this.popKeymap(keymap);
    };
});

(function(){

    var $menu = null, $selectedIndex = null, $selectedItem = null;
    function popupCompletionMenu(frame, list) {
        let activeElement = document.activeElement;
        let ymacs = this.ymacs; // `this` is the minibuffer
        $menu = new Ymacs_Popup();
        $menu.addClass("Ymacs_Completions");
        list.forEach((label, index) => {
            let value = label;
            if (typeof label != "string") {
                value = label.value;
                label = label.label;
            }
            let el = DOM.fromHTML(`<div class="Ymacs_Menu_Item" data-value="${DOM.htmlEscape(value)}"
                                        data-index="${index}">${DOM.htmlEscape(label)}</div>`);
            $menu.add(el);
        });
        DOM.on($menu.getContentElement(), {
            click: ev => {
                activeElement.focus();
                let item = ev.target;
                if (!DOM.hasClass(item, "Ymacs_Menu_Item")) return;
                select(+item.dataset.index);
                handle_enter.call(this);
                handle_enter.call(this);
            },
        });
        ymacs.add($menu);
        select(0);

        this.pushKeymap(KEYMAP_POPUP_ACTIVE);
        $menu.addEventListener("onDestroy", () => this.popKeymap(KEYMAP_POPUP_ACTIVE));
    }

    function killMenu() {
        if ($menu) $menu.destroy();
        $menu = null;
        $selectedItem = null;
        $selectedIndex = null;
    }

    function select(idx) {
        let cont = $menu.getContentElement();
        let items = [...cont.querySelectorAll(".Ymacs_Menu_Item")];
        let n = items.length;
        $selectedIndex = ((idx % n) + n) % n;
        items.forEach(el => {
            let current = el.dataset.index == $selectedIndex;
            if (current) $selectedItem = el;
            DOM.condClass(el, current, "selected");
        });
        $selectedItem.scrollIntoView({ block: "center" });
    }

    function read_with_continuation(completions, cont, validate) {
        this.whenMinibuffer(function(mb){
            var changed_vars = mb.setq({
                completion_list: completions,
                minibuffer_validation: (what, cont2) => {
                    if (what == null)
                        what = mb.cmd("minibuffer_contents");
                    if (validate)
                        validate.call(this, mb, what, cont2);
                    else
                        cont2(true); // accept anything by default
                },
                minibuffer_continuation: what => {
                    mb.setq(changed_vars);
                    if (cont)
                        cont.call(this, what);
                },
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
                if (files[name].type == "directory") {
                    return name + "/";
                } else {
                    return name;
                }
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
                    var prefix = common_prefix(completions);
                    if (prefix != partial) {
                        mb.cmd("minibuffer_replace_input", dir + prefix);
                        cont(null);
                    } else if (completions.length == 1) {
                        cont([str]);
                    } else {
                        completions = completions.map(function(name){
                            return { label: name, value: dir + name };
                        });
                        console.log(completions);
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
            var completions = Object.keys(this.COMMANDS).filter(cmd => this.COMMANDS[cmd].ymacsInteractive).sort();
            read_with_continuation.call(this, completions, cont, function(mb, name, cont2){
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
                var bufferNames = ymacs.buffers.map(b => b.name);
                bufferNames.push(bufferNames.shift());
                read_with_continuation.call(this, bufferNames, cont);
                //handle_tab.call(this);
            });
        },

        minibuffer_read_string: function(completions, cont) {
            read_with_continuation.call(this, completions, cont);
        },

        minibuffer_read_variable: function(cont) {
            var tmp = Object.assign({}, this.globalVariables, this.variables);
            var completions = Object.keys(tmp).filter(name => !/^\*/.test(name)).sort();
            read_with_continuation.call(this, completions, function(name) {
                let val = this.getq(name);
                this.signalInfo(`Current value of <b>${DOM.htmlEscape(name)}</b> = <b>${DOM.htmlEscape(val)}</b>`, true, 3000);
                return cont.apply(this, arguments);
            });
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
                        var prefix = common_prefix(a);
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
                    a = a.filter(cmd => re.test(cmd));
                    complete(a);
                }
                else
                    complete(a);
            });
        },

        minibuffer_complete_and_exit: function() {
            this.whenMinibuffer(mb => {
                mb.getq("minibuffer_validation").call(mb, null, valid => {
                    if (valid)
                        mb.cmd("minibuffer_keyboard_quit", this.getq("minibuffer_continuation"));
                });
            });
        },

        minibuffer_keyboard_quit: function(cont) {
            this.whenMinibuffer(function(mb){
                var text = this.cmd("minibuffer_contents");
                mb.setCode("");
                this.ymacs.setInputFrame(this.ymacs.getActiveFrame());
                this.ymacs.getActiveFrame().focus();
                if (!cont) {
                    mb.callHooks("abort");
                }
                setTimeout(() => {
                    if (cont)
                        cont.call(this, text);
                    this.getPrefixArg();
                }, 1);
            });
            killMenu();
        }

    });

    function handle_arrow_down() {
        if ($menu) {
            select($selectedIndex + 1);
        }
    };

    function handle_arrow_up() {
        if ($menu) {
            select($selectedIndex - 1);
        }
    };

    function handle_enter() {
        if ($menu) {
            if ($selectedItem) {
                this.cmd("minibuffer_replace_input", $selectedItem.dataset.value);
                killMenu();
            } else {
                this.signalError("Select something...");
            }
        } else {
            this.cmd("minibuffer_complete_and_exit");
        }
    };

    function handle_tab() {
        if (!$menu)
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
        "Tab"                                : handle_tab,
        "Enter"                              : handle_enter,
        "Home && C-a"                        : handle_home,
        "S-Home && S-C-a"                    : Ymacs_Interactive("^", handle_home_mark)
    };

    Ymacs_Keymap.define("minibuffer", Object.assign({
        "C-g && Escape" : "minibuffer_keyboard_quit"
    }, DEFAULT_KEYS));

    var KEYMAP_POPUP_ACTIVE = Ymacs_Keymap.define(null, Object.assign({
        "S-Tab"                                 : handle_s_tab,
        "ArrowDown && ArrowRight && C-n && C-f" : handle_arrow_down,
        "ArrowUp && ArrowLeft && C-p && C-b"    : handle_arrow_up,
        "Escape"                                : function() {
            killMenu();
        }
    }, DEFAULT_KEYS));
    KEYMAP_POPUP_ACTIVE.defaultHandler = [ function() {
        killMenu();
        return false; // say it's not handled though
    } ];

})();
