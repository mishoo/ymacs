/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT

import { Ymacs_Buffer } from "./ymacs-buffer.js";
import { Ymacs_Keymap } from "./ymacs-keymap.js";
import { DOM, common_prefix, fuzzy_filter, delayed } from "./ymacs-utils.js";
import { Ymacs_Popup } from "./ymacs-popup.js";
import { Ymacs_Interactive } from "./ymacs-interactive.js";

var $menu = null, $selectedIndex = null, $selectedItem = null;
function popupCompletionMenu(frame, list, { noError = false, noPrefix = false } = {}) {
    killMenu();
    let activeElement = document.activeElement;
    let ymacs = this.ymacs; // `this` is the minibuffer
    $menu = new Ymacs_Popup();
    $menu.addClass("with-arrow");
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
        },
    });
    ymacs._popupAtCaret($menu.getElement());
    select(0);

    // XXX: this is some more aggressive, ivy-like completion, but
    // it's not working very well with filename completion.
    let onChange = this.getq("ivy_completion") && delayed(() => {
        killMenu();
        this.cmd("minibuffer_complete", { noError, noPrefix });
    }, 1);

    this.pushKeymap(KEYMAP_POPUP_ACTIVE);
    if (onChange) this.addEventListener("onChange", onChange);
    $menu.addEventListener("onDestroy", () => {
        this.popKeymap(KEYMAP_POPUP_ACTIVE);
        if (onChange) this.removeEventListener("onChange", onChange);
    });
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
    $selectedItem.scrollIntoView({ block: "nearest" });
}

export function read_with_continuation(completions, cont, validate) {
    this.whenMinibuffer(function(mb){
        let ivy = completions !== filename_completion;
        var changed_vars = mb.setq({
            ivy_completion: ivy,
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
        mb.cmd("minibuffer_complete", { noError: true, noPrefix: !ivy });
    });
}

function filename_completion(mb, str, cont, { noPrefix = false } = {}) {
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
            for (let f of Object.keys(files)) {
                if (f.indexOf(partial) == 0) {
                    completions.push(add_trailing_slash_to_dir(f));
                }
            }
            if (completions.length == 0) {
                cont([]);
            } else {
                var prefix = common_prefix(completions);
                if (prefix != partial && !noPrefix) {
                    mb.cmd("minibuffer_replace_input", dir + prefix);
                    cont(completions.map(name => ({ label: name, value: dir + name })));
                } else if (completions.length == 1) {
                    cont([ str ]);
                } else {
                    cont(completions.map(name => ({ label: name, value: dir + name })));
                }
            }
        }
    });
}

Ymacs_Buffer.newCommands({

    minibuffer_prompt: function(prompt, nofocus) {
        this.whenMinibuffer(function(mb){
            var f = this.getMinibufferFrame();
            this.ymacs.setInputFrame(f);
            mb.prompt(prompt);
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
        var commandNames = Array.hashKeys(Ymacs_Buffer.COMMANDS).sort();
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

    minibuffer_read_string: function(completions, cont, validate) {
        read_with_continuation.call(this, completions, cont, validate);
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
                self.ymacs.fs_fileType(name, function (type){
                    if (type != "file") {
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
            read_with_continuation.call(self, filename_completion, cont, function(mb, name, cont2){
                self.ymacs.fs_fileType(name, function (type){
                    cont2(type != "directory");
                });
            });
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
            return mb.promptMarker.getPosition();
        });
    },

    minibuffer_contents: function() {
        return this.whenMinibuffer(function(mb){
            return mb._bufferSubstring(mb.promptMarker);
        });
    },

    minibuffer_replace_input: function(value) {
        this.whenMinibuffer(function(mb){
            mb._replaceText(mb.promptMarker, mb.getCodeSize(), value);
            this.getMinibufferFrame().redrawCaret(true);
        });
    },

    minibuffer_replace_input_by_current_dir: function (cont) {
        this.whenYmacs(function(ymacs){
            var self = this;
            var name = ymacs.getActiveBuffer().name;
            var dir = name.slice(0, name.lastIndexOf("/")+1);
            ymacs.fs_getDirectory(dir, function (info){
                if (info && Object.keys(info).length > 0) {
                    ymacs.fs_remapDir(dir, function (dir) {
                        self.cmd("minibuffer_replace_input", dir);
                        self.cmd("minibuffer_complete", { noError: true, noPrefix: true });
                        cont();
                    });
                } else {
                    self.cmd("minibuffer_replace_input", "");
                    cont();
                }
            });
        });
    },

    minibuffer_complete: function({ noError = false, noPrefix = false } = {}) {
        var self = this;
        self.whenMinibuffer(function(mb){
            let str = mb.cmd("minibuffer_contents");
            let a = mb.getq("completion_list");

            function complete(a) {
                if (!a || a.length == 0) {
                    if (!noError) mb.signalError("No completions", false, 2000);
                } else {
                    popupCompletionMenu.call(mb, self.getMinibufferFrame(), a, { noError, noPrefix });
                }
            }

            if (a instanceof Function) {
                a.call(self, mb, str, function (a) {
                    if (a) complete(a);
                }, { noPrefix });
            }
            else if (a && a.length > 0) {
                complete(fuzzy_filter(a, str));
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
                else
                    mb.cmd("minibuffer_complete");
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
}

function handle_arrow_up() {
    if ($menu) {
        select($selectedIndex - 1);
    }
}

function handle_popup_home() {
    if ($menu) {
        select(0);
    }
}

function handle_popup_end() {
    if ($menu) {
        select(-1);
    }
}

function handle_enter() {
    if ($menu) {
        let item = $selectedItem;
        if (item) {
            killMenu();
            this.cmd("minibuffer_replace_input", item.dataset.value);
            this.cmd("minibuffer_complete_and_exit");
        } else {
            this.signalError("Select something...");
        }
    } else {
        this.cmd("minibuffer_complete_and_exit");
    }
}

function handle_tab() {
    if (!$menu)
        this.cmd("minibuffer_complete");
    else
        handle_arrow_down.call(this);
}

function handle_s_tab() {
    handle_arrow_up.call(this);
}

function handle_home() {
    this.cmd("goto_char", this.promptMarker);
}

function handle_home_mark() {
    this.ensureTransientMark();
    this.cmd("goto_char", this.promptMarker);
    this.ensureTransientMark();
}

function handle_tilde() {
    if (this.getq("completion_list") === filename_completion) {
        if (this.cmd("looking_back", "/")) {
            killMenu();
            this.cmd("minibuffer_replace_input", "~/");
            this.cmd("minibuffer_complete", { noError: true, noPrefix: true });
            return;
        }
    }
    this.cmd("self_insert_command");
}

var DEFAULT_KEYS = {
    "Tab"                                : handle_tab,
    "Enter"                              : handle_enter,
    "Home && C-a"                        : handle_home,
    "~"                                  : handle_tilde,
    "S-Home && S-C-a"                    : Ymacs_Interactive("^", handle_home_mark),
};

var KEYMAP_MB_ACTIVE = Ymacs_Keymap.define(null, Object.assign({
    "C-g && Escape" : "minibuffer_keyboard_quit"
}, DEFAULT_KEYS));

var KEYMAP_POPUP_ACTIVE = Ymacs_Keymap.define(null, Object.assign({
    "S-Tab"                                 : handle_s_tab,
    "ArrowDown && ArrowRight && C-n && C-f" : handle_arrow_down,
    "ArrowUp && ArrowLeft && C-p && C-b"    : handle_arrow_up,
    "PageDown"                              : handle_arrow_down,
    "PageUp"                                : handle_arrow_up,
    "C-End && M->"                          : handle_popup_end,
    "C-Home && M-<"                         : handle_popup_home,
    "Escape"                                : function() {
        killMenu();
    }
}, DEFAULT_KEYS));
KEYMAP_POPUP_ACTIVE.defaultHandler = [ function() {
    if (!this.getq("ivy_completion")) killMenu();
    return false; // say it's not handled though
} ];

Ymacs_Buffer.newMode("minibuffer_mode", function(){
    this.pushKeymap(KEYMAP_MB_ACTIVE);
    return function() {
        this.popKeymap(KEYMAP_MB_ACTIVE);
    };
});
