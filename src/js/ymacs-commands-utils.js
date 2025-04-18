/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT

import { zeroPad, getYmacsThemes } from "./ymacs-utils.js";
import { Ymacs_Buffer } from "./ymacs-buffer.js";
import { Ymacs_Interactive } from "./ymacs-interactive.js";
import { read_with_continuation } from "./ymacs-minibuffer.js";

Ymacs_Buffer.newCommands({

    get_region: function() {
        return this.getRegion();
    },

    figure_out_mode: function(code) {
        if (!code)
            code = this.getCode();
        var lines = code.split(/\n/);
        if (lines.length > 4)
            lines.splice(2, lines.length - 4);
        for (let line of lines) {
            let m = /-\*-\s*(.*?)\s*-\*-/i.exec(line);
            if (m) return m[1];
        }
    },

    // TODO: the mapping from extension to mode should be defined
    // with each mode.
    mode_from_name: function(name) {
        if (!name) name = this.name;
        var ext = (/\.[^.]+$/.exec(name) || [""])[0];
        switch (ext) {
          case ".css":
            return "css";

          case ".js":
            return "javascript";

          case ".lisp":
          case ".scm":
          case ".el":
            return "lisp";

          case ".md":
            return "markdown";

          case ".xml":
            return "xml";

          case ".html":
            return "html";

          case ".twig":
            return "twig_html";
        }
        return null;
    },

    set_buffer_mode: function(mode) {
        if (!mode)
            mode = this.cmd("figure_out_mode") || this.cmd("mode_from_name");
        if (mode) {
            if (this.COMMANDS[mode]) {
                this.cmd(mode, true);
            } else if (this.COMMANDS[mode + "_mode"]) {
                this.cmd(mode + "_mode", true);
            }
        }
    },

    cperl_lineup: Ymacs_Interactive("r", function(begin, end){
        this.cmd("save_excursion", function(){
            var rcend = this._positionToRowCol(end), max = 0, lines = [];
            this.cmd("goto_char", begin);
            this.cmd("forward_whitespace", true);
            var ch = this.charAt();
            if (ch.toLowerCase() != ch.toUpperCase()) {
                this.signalError("Cannot lineup here");
                return;
            }
            while (this._rowcol.row <= rcend.row) {
                var pos = this.getLine().indexOf(ch);
                if (pos >= 0) {
                    if (pos > max)
                        max = pos;
                    lines.push([ this._rowcol.row, pos ]);
                }
                if (!this.cmd("forward_line"))
                    break;
            }
            ++max;
            lines.forEach(l => {
                this.cmd("goto_char", this._rowColToPosition(l[0], l[1]));
                this.cmd("insert", " ".repeat(max - l[1]));
            });
        });
    }),

    htmlize_region: Ymacs_Interactive("r\nP", function(begin, end, lineNum) {
        this.tokenizer.finishParsing();
        var row = this._positionToRowCol(begin).row;
        var html = "";
        var line = row;
        var pad;
        if (lineNum && !lineNum.empty)
            line = parseInt(lineNum, 10);
        end = this._positionToRowCol(end).row;
        pad = String(end).length;
        while (row <= end) {
            html += "<div class='line'>";
            if (lineNum)
                html += "<span class='line-number'>" + zeroPad(line, pad, " ") + "</span>";
            ++line;
            html += this._textProperties.getLineHTML(row, this.code[row], null) + "</div>\n";
            ++row;
        }
        var tmp = this.ymacs.switchToBuffer("*Htmlize*");
        tmp.setCode(html);
        tmp.cmd("xml_mode", true);
    }),

    execute_extended_command: Ymacs_Interactive("^CM-x ", function(cmd) {
        this.callInteractively(cmd);
    }),

    set_variable: Ymacs_Interactive("vSet variable: \nsTo value: ", function(variable, value) {
        var tmp = parseFloat(value);
        if (!isNaN(tmp))
            value = tmp;
        this.setq(variable, value);
    }),

    eval_string: Ymacs_Interactive("^MEval string: ", function(code){
        try {
            var variables = [
                this,      // buffer
                this.ymacs // ymacs
            ];
            code = new Function("buffer", "ymacs", code);
            code.apply(this, variables);
            this.clearTransientMark();
        } catch(ex) {
            this.signalError(ex.type + ": " + ex.message);
            if (window.console)
                console.log(ex);
        }
    }),

    eval_region: Ymacs_Interactive("^r", function(begin, end) {
        this.cmd("eval_string", this.cmd("buffer_substring", begin, end));
    }),

    eval_buffer: Ymacs_Interactive(function(){
        this.cmd("eval_string", this.getCode());
    }),

    toggle_line_numbers: Ymacs_Interactive("^", function(){
        this.ymacs.toggleClass("Ymacs-line-numbers");
    }),

    save_file: Ymacs_Interactive("FWrite file: ", function(name){
        this.ymacs.ls_setFileContents(name, this.getCode());
        this.signalInfo("Saved in local storage");
    }),

    load_file: Ymacs_Interactive("fFind file: ", function(name){
        var code = this.ymacs.ls_getFileContents(name);
        var buffer = this.ymacs.createBuffer({ name: name });
        buffer.setCode(code);
        buffer.cmd("set_buffer_mode");
        buffer.cmd("switch_to_buffer", name);
    }),

    find_file: Ymacs_Interactive("FFind file: ", function(name) {
        var self = this;
        name = self.ymacs.fs_normalizePath(name);
        self.ymacs.fs_fileType(name, function (type) {
            if (type == "directory") {
                self.signalInfo("Can't open directory");
            } else {
                self.ymacs.fs_getFileContents(name, true, function (code, stamp) {
                    var buffer = self.ymacs.getBuffer(name);

                    function find_file() {
                        buffer.setCode(code || "");
                        buffer.stamp = stamp;
                        buffer.dirty(false);
                        buffer.cmd("set_buffer_mode");
                        buffer.cmd("switch_to_buffer", name);
                    }

                    if (buffer) {
                        if (stamp == null) {
                            find_file();
                        } else if (buffer.stamp == stamp) {
                            buffer.cmd("switch_to_buffer", name);
                        } else {
                            var msg = "File " + name + " changed on disk.  "
                                + (buffer.dirty() ? "Discard your edits?" : "Reread from disk?");
                            buffer.cmd("minibuffer_yn", msg, function (yes) {
                                if (yes)
                                    find_file();
                            });
                        }
                    } else {
                        buffer = self.ymacs.createBuffer({ name: name, stamp: stamp });
                        if (code == null)
                            self.signalInfo("New file");
                        find_file();
                    }
                });
            }
        });
    }),

    write_file: Ymacs_Interactive("FWrite file: ", function(name){
        var self = this;

        function write_file() {
            self.ymacs.fs_setFileContents(name, self.getCode(), null, function (stamp) {
                self.cmd("rename_buffer", name);
                self.dirty(false);
                self.stamp = stamp; // refresh stamp
                self.signalInfo("Wrote "+name);
            });
        }

        var buffer = self.ymacs.getBuffer(name);
        if (!buffer)
            write_file();
        else {
            var msg = "A buffer is visiting "+name+"; proceed?";
            buffer.cmd("minibuffer_yn", msg, function (yes) {
                if (yes) {
                    self.ymacs.killBuffer(buffer);
                    write_file();
                }
            });
        }
    }),

    save_some_buffers: function () {
        this.cmd("save_some_buffers_with_continuation", true, function () { });
    },

    save_some_buffers_with_continuation: function (ask, cont) {

        var bufs = this.ymacs.buffers.slice(); // get copy of buffers

        function loop(saved) {
            if (bufs.length > 0)
                bufs.shift().cmd("save_buffer_with_continuation", ask, loop);
            else
                cont();
        }

        loop(false);
    },

    save_buffer_with_continuation: function (ask, cont) {

        var self = this;

        function did_save(stamp) {
            self.dirty(false);
            self.stamp = stamp; // refresh stamp
            cont(true);
        }

        function do_save() {
            self.ymacs.fs_setFileContents(self.name, self.getCode(), self.stamp, function (stamp) {
                if (stamp != null) {
                    did_save(stamp);
                } else {
                    self.cmd("minibuffer_yn", self.name + " has changed since visited or saved.  Save anyway?", function (yes) {
                        if (!yes) {
                            cont(false);
                        } else {
                            self.ymacs.fs_setFileContents(self.name, self.getCode(), null, function (stamp) {
                                did_save(stamp);
                            });
                        }
                    });
                }
            });
        }

        if (!self.dirty() || (ask && self.name.match(/^\*.*\*$/))) {
            cont(false);
        } else if (!ask) {
            do_save();
        } else {
            self.cmd("minibuffer_yn", "Save file " + self.name + "?", function (yes) {
                if (!yes) {
                    cont(false);
                } else {
                    do_save();
                }
            });
        }
    },

    save_buffer: Ymacs_Interactive("", function () {
        var self = this;
        if (self.dirty())
            self.cmd("save_buffer_with_continuation", false, function (saved) {
                if (saved)
                    self.signalInfo("Wrote "+self.name);
            });
        else
            self.signalInfo("No changes need to be saved");
    }),

    delete_file: Ymacs_Interactive("fDelete file: ", function(name){
        var self = this;
        self.ymacs.fs_deleteFile(name, function () {
            self.signalInfo("Deleted "+name);
        });
    }),

    eval_file: Ymacs_Interactive("fEval file: ", function(name){
        var self = this;
        self.ymacs.fs_getFileContents(name, false, function (code, stamp) {
            self.cmd("eval_string", code);
        });
    }),

    request_full_screen: Ymacs_Interactive(async function(){
        try {
            this.ymacs.requestFullScreen();
        } catch(ex) {
            console.error(ex);
            this.signalError("Full-screen denied", false, 3000);
        }
    }),

    set_color_theme: Ymacs_Interactive(function(theme){
        if (theme) {
            this.ymacs.setColorTheme(theme);
        } else {
            let names = getYmacsThemes().sort();
            this.cmd("minibuffer_prompt", "Color theme: ");
            read_with_continuation.call(this, names, theme => {
                this.ymacs.setColorTheme(theme);
            }, (mb, theme, cont) => {
                theme = theme.trim();
                if (!theme) {
                    mb.cmd("minibuffer_complete");
                } else if (names.indexOf(theme) < 0) {
                    mb.signalError("No such theme");
                    return cont(false);
                } else {
                    return cont(true);
                }
            });
        }
    }),

    toggle_bar_cursor: Ymacs_Interactive(function(){
        this.ymacs.toggleBarCursor();
    }),

});
