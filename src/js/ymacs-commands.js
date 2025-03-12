/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT

import { DOM, delayed, formatBytes } from "./ymacs-utils.js";
import { Ymacs_Keymap } from "./ymacs-keymap.js";
import { Ymacs_Keymap_UniversalArgument } from "./ymacs-keymap-emacs.js";
import { Ymacs_Buffer } from "./ymacs-buffer.js";
import { Ymacs_Exception } from "./ymacs-exception.js";
import { Ymacs_Interactive, Ymacs_Interactive_X } from "./ymacs-interactive.js";

function TMPL_CHAR_INFO({
    ch, code, codeHex, point, sizeKB
}) {
    return `
<table>
  <tr><td style='text-align: right; font-weight: bold'>Char:</td><td><tt> ${ch} </tt></td></tr>
  ${code != null ? `
    <tr><td style='text-align: right; font-weight: bold'>Char code:</td><td> ${code} / 0x${codeHex} </td></tr>
  ` : ""}
  <tr><td style='text-align: right; font-weight: bold'>Position:</td><td> ${point} </td></tr>
  <tr><td style='text-align: right; font-weight: bold'>Buffer size:</td><td> ${sizeKB} </td></tr>
</table>`;
}

let help_keymap = Ymacs_Keymap.define("help", {
    "q": "bury_buffer",
});

let read_register_keymap = Ymacs_Keymap.define("read_register", {
    "Escape && C-g": function() {
        this.popKeymap(read_register_keymap);
        this.setMinibuffer("");
    }
});
read_register_keymap.defaultHandler = [ function(){
    var ev = this.interactiveEvent();
    var ch = ev.key;
    if (ch.length != 1) {
        this.signalError("Non-character input event");
    } else {
        this.getq("read_register_callback")(ch);
        this.setq("read_register_callback");
    }
    this.popKeymap(read_register_keymap);
    this.setMinibuffer("");
    return true;
} ];

Ymacs_Buffer.newCommands({

    forward_char: Ymacs_Interactive("p", function(x) {
        if (x == null) x = 1;
        return this.cmd("goto_char", this.point() + x);
    }),

    backward_char: Ymacs_Interactive("p", function(x) {
        if (x == null) x = 1;
        return this.cmd("forward_char", -x);
    }),

    forward_line: Ymacs_Interactive("p", function(x) {
        if (x == null) x = 1;
        var rc = this._rowcol;
        if (!/^(forward|backward)_line$/.test(this.previousCommand)) {
            this.setq("line_movement_requested_col", rc.col);
        }
        var ret = this.cmd("goto_char",
                           this._rowColToPosition(rc.row + x,
                                                  Math.max(rc.col,
                                                           this.getq("line_movement_requested_col"))));
        if (!ret)
            this.setq("line_movement_requested_col", rc.col);
        return ret;
    }),

    backward_line: Ymacs_Interactive("p", function(x) {
        if (x == null) x = 1;
        return this.cmd("forward_line", -x);
    }),

    forward_whitespace: Ymacs_Interactive("P", function(noLine) {
        var re = noLine ? /[^\x20\t\xA0]/g : /[^\s]/g;
        if (this.cmd("search_forward_regexp", re)) {
            this.cmd("backward_char");
            return true;
        } else if (!noLine)
            return this.cmd("end_of_buffer");
    }),

    backward_whitespace: Ymacs_Interactive("P", function(noLine) {
        var re = noLine ? /[^\x20\t\xA0]/g : /[^\s]/g;
        if (this.cmd("search_backward_regexp", re)) {
            this.cmd("forward_char");
            return true;
        } else if (!noLine)
            return this.cmd("beginning_of_buffer");
    }),

    beginning_of_line: Ymacs_Interactive(function() {
        return this.cmd("goto_char", this._rowColToPosition(this._rowcol.row, 0));
    }),

    back_to_indentation: Ymacs_Interactive(function() {
        var rc = this._rowcol, line = this.code[rc.row], m = /\S/.exec(line);
        if (m)
            return this.cmd("goto_char", this._rowColToPosition(rc.row, m.index));
    }),

    beginning_of_indentation_or_line: Ymacs_Interactive(function() {
        return this.cmd("back_to_indentation") || this.cmd("beginning_of_line");
    }),

    end_of_line: Ymacs_Interactive(function() {
        var rc = this._rowcol;
        return this.cmd("goto_char", this._rowColToPosition(rc.row, this.code[rc.row].length));
    }),

    beginning_of_buffer: Ymacs_Interactive(function() {
        return this.cmd("goto_char", 0);
    }),

    end_of_buffer: Ymacs_Interactive(function() {
        return this.cmd("goto_char", this.getCodeSize());
    }),

    eob_p: function() {
        return this.point() == this.getCodeSize();
    },

    bob_p: function() {
        return this.point() == 0;
    },

    eol_p: function() {
        var rc = this._positionToRowCol(this.point());
        return rc.col == this.code[rc.line].length;
    },

    bol_p: function() {
        return this._positionToRowCol(this.point()).col == 0;
    },

    backward_delete_char: Ymacs_Interactive("^p", function(n){
        if (!this.deleteTransientRegion()) {
            if (n == null) n = 1;
            var pos = this.point();
            if (pos > 0)
                this._deleteText(pos - n, pos);
        }
    }),

    delete_char: Ymacs_Interactive("^p", function(n){
        if (!this.deleteTransientRegion()) {
            if (n == null) n = 1;
            var pos = this.point();
            this._deleteText(pos, pos + n);
        }
    }),

    delete_whitespace: Ymacs_Interactive("^P", function(noLine) {
        if (!this.deleteTransientRegion()) {
            var p = this.point();
            if (this.cmd("forward_whitespace", noLine)) {
                this._deleteText(p, this.point());
                return true;
            }
        }
    }),

    backward_delete_whitespace: Ymacs_Interactive("^P", function(noLine) {
        if (!this.deleteTransientRegion()) {
            var p = this.point();
            if (this.cmd("backward_whitespace", noLine)) {
                this._deleteText(this.point(), p);
                return true;
            }
        }
    }),

    delete_indentation: Ymacs_Interactive("P", function(nextLine) {
        if (nextLine)
            this.cmd("forward_line");
        this.cmd("back_to_indentation");
        this.cmd("backward_delete_whitespace");
        this.cmd("insert", " ");
    }),

    universal_argument: Ymacs_Interactive("^", function(){
        this.pushKeymap(Ymacs_Keymap_UniversalArgument);
        if (!this.isMinibuffer)
            this.setMinibuffer("C-u");
    }),

    overwrite_mode: Ymacs_Interactive(function() {
        this.resetOverwriteMode();
    }),

    self_insert_command: Ymacs_Interactive("^p", function(repeat) {
        var ev = this.interactiveEvent();
        var ch = ev.key;
        var rc = this._rowcol;
        if (ch.length == 1 && !ev.altKey && !ev.ctrlKey) {
            this.deleteTransientRegion();
            if (repeat != null)
                ch = ch.repeat(repeat);
            if (this.overwriteMode) {
                var line = this.code[rc.row], left = line.length - rc.col;
                if (left > 0)
                    this.cmd("delete_char", Math.min(left, repeat || 1));
            }
            this.cmd("insert", ch);
            return true;
        }
        return false;
    }),

    newline: Ymacs_Interactive("^p", function(n){
        if (n == null) n = 1;
        this.deleteTransientRegion();
        this.cmd("insert", "\n".repeat(n));
    }),

    newline_and_indent: Ymacs_Interactive("^p", function(n){
        if (n) {
            this.cmd("newline", n);
        } else {
            this.cmd("backward_delete_whitespace", true);
            this.cmd("newline");
            this.cmd("indent_line");
        }
    }),

    indent_line: Ymacs_Interactive("P", function(noEmpty) {
        if (!this.tokenizer) {
            this.cmd("insert", " ".repeat(this.getq("indent_line")));
        } else {
            let indent = this.tokenizer.getIndentation(this._rowcol.row, this);
            if (indent != null) {
                let line = this.getLine();
                if (!noEmpty || /\S/.test(line)) {
                    let rc = this._rowcol;
                    let pos = this.point() - rc.col;
                    let ci = /[^\S\r\n]*/.exec(line)[0].length;
                    if (ci > indent) {
                        this._deleteText(pos, pos + ci - indent);
                    } else if (indent > ci) {
                        this._insertText(" ".repeat(indent - ci), pos);
                    }
                    if (rc.col <= ci) {
                        this.cmd("goto_char", pos + indent);
                    }
                }
            }
        }
    }),

    indent_region: Ymacs_Interactive("r", function(begin, end) {
        if (end < begin) [ begin, end ] = [ end, begin ];
        this.cmd("save_excursion", function() {
            var m = this.createMarker(end);
            this.cmd("goto_char", begin);
            while (this.point() < m.getPosition()) {
                this.cmd("indent_line", true);
                this.cmd("beginning_of_line");
                if (!this.cmd("forward_line"))
                    break;
            };
            m.destroy();
        });
    }),

    make_marker: function(pos) {
        return this.createMarker(pos);
    },

    looking_at: function() {
        return this.looking_at.apply(this, arguments);
    },

    looking_back: function() {
        return this.looking_back.apply(this, arguments);
    },

    search_forward: Ymacs_Interactive("sSearch: ", function(str, bound) {
        var code = this.getCode(), point = this.point();
        if (this.getq("case_fold_search")) {
            code = code.toLowerCase();
            str = str.toLowerCase();
        }
        var pos = code.indexOf(str, point);
        if (pos >= 0 && (bound == null || pos <= bound)) {
            return this.cmd("goto_char", pos + str.length);
        }
    }),

    search_backward: Ymacs_Interactive("sSearch backward: ", function(str, bound) {
        var code = this.getCode(), point = this.point();
        if (this.getq("case_fold_search")) {
            code = code.toLowerCase();
            str = str.toLowerCase();
        }
        var pos = code.lastIndexOf(str, point);
        if (pos == point)
            pos = code.lastIndexOf(str, point - 1);
        if (pos >= 0 && pos != point && (bound == null || pos >= bound)) {
            return this.cmd("goto_char", pos);
        }
    }),

    make_regexp: function(rx) {
        if (!(rx instanceof RegExp)) {
            var matchCase = !this.getq("case_fold_search");
            try {
                rx = new RegExp(rx, matchCase ? "mug" : "mugi");
            } catch(ex) {
                throw new Ymacs_Exception("Invalid regexp");
            }
        }
        return rx;
    },

    search_forward_regexp: Ymacs_Interactive("sRegExp search: ", function(rx) {
        rx = this.cmd("make_regexp", rx);
        let pos = rx.lastIndex = this.point();
        let m = rx.exec(this.getCode());
        if (m && rx.lastIndex != pos) {
            m.after = rx.lastIndex;
            this.matchData = m;
            this.cmd("goto_char", rx.lastIndex);
            return true;
        }
    }),

    search_backward_regexp: Ymacs_Interactive("sBackward RegExp search: ", function(rx) {
        rx = this.cmd("make_regexp", rx);
        var m = this.lastIndexOfRegexp(this.getCode(), rx, this.point());
        if (m && m.index != this.point()) {
            this.cmd("goto_char", m.index);
            return true;
        }
    }),

    forward_word: Ymacs_Interactive_X(function(){
        var syntax_word = this.getq("syntax_word"), end = false;
        while (!end && !syntax_word.test(this.charAt()))
            if (!this.cmd("forward_char"))
                end = true;
        while (!end && syntax_word.test(this.charAt()))
            if (!this.cmd("forward_char"))
                end = true;
    }),

    backward_word: Ymacs_Interactive_X(function(){
        var syntax_word = this.getq("syntax_word"), end = false;
        while (!end && !syntax_word.test(this.charAt(-1)))
            if (!this.cmd("backward_char"))
                end = true;
        while (!end && syntax_word.test(this.charAt(-1)))
            if (!this.cmd("backward_char"))
                end = true;
    }),

    forward_paragraph: Ymacs_Interactive_X(function(){
        let rx = this.getq("syntax_paragraph_sep");
        this.cmd("beginning_of_line");
        this.cmd("backward_char");
        if (this.cmd("looking_at", rx)) {
            this.cmd("goto_char", this.cmd("match_end"));
        }
        if (this.cmd("search_forward_regexp", rx)) {
            this.cmd("goto_char", this.cmd("match_beginning") + 1);
        } else {
            this.cmd("end_of_buffer");
        }
    }),

    backward_paragraph: Ymacs_Interactive_X(function(){
        let rx = this.getq("syntax_paragraph_sep");
        this.cmd("beginning_of_line");
        if (this.cmd("looking_back", rx)) {
            this.cmd("goto_char", this.cmd("match_beginning"));
        }
        if (this.cmd("search_backward_regexp", rx)) {
            this.cmd("goto_char", this.cmd("match_end"));
        } else {
            this.cmd("beginning_of_buffer");
        }
    }),

    mark_paragraph: Ymacs_Interactive("^r", function(begin, end){
        if (!this.transientMarker) {
            this.cmd("forward_paragraph");
            this.setMark(this.point());
            this.ensureTransientMark();
            this.cmd("backward_paragraph");
        }
        else this.cmd("save_excursion", function(){
            if (this.transientMarker)
                this.cmd("goto_char", end);
            this.ensureTransientMark();
            this.cmd("forward_paragraph");
            this.setMark(this.point());
            this.transientMarker.swap(this.caretMarker);
        });
        this.ensureTransientMark();
        this.setq("sticky_mark", true);
    }),

    transpose_words: Ymacs_Interactive_X(function() {
        // if we're in the middle of a word, some
        // weird things happen; better skip it, just
        // like Emacs does.
        this.cmd("backward_char");
        if (this.getq("syntax_word").test(this.charAt()))
            this.cmd("forward_word");

        var a = [];
        this.cmd("forward_word"); a.push(this.point());
        this.cmd("backward_word"); a.push(this.point());
        this.cmd("backward_word"); a.push(this.point());
        this.cmd("forward_word"); a.push(this.point());
        this.cmd("goto_char", this._swapAreas(a));
    }),

    transpose_lines: Ymacs_Interactive_X(function() {
        var a = [];
        this.cmd("backward_line");
        this.cmd("beginning_of_line"); a.push(this.point());
        this.cmd("end_of_line"); a.push(this.point());
        this.cmd("forward_char"); a.push(this.point());
        this.cmd("end_of_line"); a.push(this.point());
        this.cmd("goto_char", this._swapAreas(a) + 1);
    }),

    transpose_chars: Ymacs_Interactive_X(function() {
        var pos = this.point();
        if (this.cmd("backward_char"))
            this.cmd("goto_char", this._swapAreas([ pos - 1, pos, pos, pos + 1 ]));
    }),

    kill_word: Ymacs_Interactive("^p", function(prefix) {
        if (this.transientMarker) {
            var r = this.getRegion();
            this.cmd("kill_region", r.begin, r.end);
            this.clearTransientMark();
        } else {
            var pos = this.point();
            this.cmd("forward_word", prefix == null ? 1 : prefix);
            var pos2 = this.point();
            this._killingAction(pos, pos2, false);
        }
    }),

    backward_kill_word: Ymacs_Interactive_X(function() {
        var pos = this.point();
        this.cmd("backward_word");
        var pos2 = this.point();
        this._killingAction(pos, pos2, true);
    }),

    _apply_operation_on_word: function (op, cc) {
        var pos = this.point();
        if (this.getq("syntax_word").test(this.charAt())) {
            var pos2 = this.cmd("save_excursion", function(){
                this.cmd("forward_word");
                return this.point();
            });
            var word = op.call(this._bufferSubstring(pos, pos2));
            this._deleteText(pos, pos2);
            this._insertText(word);
        } else {
            this.cmd("forward_word");
            this.cmd("backward_word");
            if (pos != this.point())
                this.cmd(cc);
        }
    },

    capitalize_word: Ymacs_Interactive_X(function() {
        this.cmd("_apply_operation_on_word", function() {
            return this.charAt(0).toUpperCase() + this.substr(1).toLowerCase();
        }, "capitalize_word");
    }),

    downcase_word: Ymacs_Interactive_X(function() {
        this.cmd("_apply_operation_on_word", String.prototype.toLowerCase, "downcase_word");
    }),

    upcase_word: Ymacs_Interactive_X(function() {
        this.cmd("_apply_operation_on_word", String.prototype.toUpperCase, "upcase_word");
    }),

    goto_char: Ymacs_Interactive("NGoto char: ", function(pos){
        return this._repositionCaret(pos);
    }),

    goto_line: Ymacs_Interactive("NGoto line: ", function(row){
        var pos = this._rowColToPosition(row - 1, 0);
        return this.cmd("goto_char", pos);
    }),

    move_to_column: Ymacs_Interactive("NMove to column: ", function(col, force){
        var rc = this._positionToRowCol(this.point());
        var text = this.code[rc.row];
        if (text.length < col) {
            if (force) {
                this.cmd("end_of_line");
                this.cmd("insert", " ".repeat(col - text.length));
            } else {
                this.cmd("end_of_line");
            }
        } else {
            this.cmd("goto_char", this._rowColToPosition(rc.row, col));
        }
    }),

    delete_region: Ymacs_Interactive("r", function(begin, end){
        this._deleteText(begin, end);
    }),

    insert: Ymacs_Interactive("sInsert text: ", function(...args){
        return this._insertText(args.join(""));
    }),

    keyboard_quit: Ymacs_Interactive(function(){
        this.clearTransientMark();
        this.setPrefixArg(undefined);
        this.setMinibuffer("");
    }),

    buffer_substring: function(begin, end) {
        if (arguments.length == 0) {
            var r = this.getRegion();
            begin = r.begin;
            end = r.end;
        }
        return this._bufferSubstring(begin, end);
    },

    kill_line: Ymacs_Interactive("^p", function(prefix) {
        if (this.transientMarker) {
            var r = this.getRegion();
            this.cmd("kill_region", r.begin, r.end);
            this.clearTransientMark();
        } else if (prefix != null) {
            this._killingAction(
                this.point(),
                this.cmd("save_excursion", () => {
                    this.cmd("forward_line", prefix);
                    this.cmd("beginning_of_line");
                    return this.point();
                })
            );
        } else {
            var pos = this.point(),
                rc = this._rowcol,
                line = this.code[rc.row],
                end = pos + line.length - rc.col;
            if (rc.row < this.code.length - 1 && this.cmd("looking_at", /\s*$/my))
                end++;
            this._killingAction(pos, end);
        }
    }),

    save_excursion: function() {
        return this._saveExcursion.apply(this, arguments);
    },

    prevent_undo: function() {
        return this._disableUndo.apply(this, arguments);
    },

    point: function() {
        return this.caretMarker.getPosition();
    },

    kill_region: Ymacs_Interactive("r", function(begin, end){
        this._killingAction(begin, end);
    }),

    copy_region_as_kill: Ymacs_Interactive("r", function(begin, end){
        this._killingAction(begin, end, false, true);
    }),

    yank: Ymacs_Interactive("^P", function(atStart){
        this.deleteTransientRegion();
        var point = this.point();
        this._insertText(this.ymacs.killRingText());
        this.setMark(point);
        if (atStart)
            this.caretMarker.swap(this.markMarker);
    }),

    yank_from_operating_system: Ymacs_Interactive(async function() {
        try {
            let code = await navigator.clipboard.readText();
            this._saveKilledText(code);
            this.callInteractively("yank");
        } catch(ex) {
            console.error(ex);
            this.signalError("Cannot read the system clipboard. Error in devtools console.");
        }
    }),

    copy_for_operating_system: Ymacs_Interactive("r", async function(begin, end) {
        try {
            let code = this.cmd("buffer_substring", begin, end);
            await navigator.clipboard.writeText(code);
        } catch(ex) {
            console.error(ex);
            this.signalError("Cannot write to system clipboard. Error in devtools console.");
        }
    }),

    yank_pop: Ymacs_Interactive(function() {
        if (/^yank/.test(this.previousCommand)) {
            this.ymacs.rotateKillRing(false);
            this._deleteText(this.caretMarker, this.markMarker);
            this.cmd("yank");
        } else {
            this.signalError("Previous command was not a yank");
        }
    }),

    yank_shift: Ymacs_Interactive(function() {
        if (/^yank/.test(this.previousCommand)) {
            this.ymacs.rotateKillRing(true);
            this._deleteText(this.caretMarker, this.markMarker);
            this.cmd("yank");
        } else {
            this.signalError("Previous command was not a yank");
        }
    }),

    mark: function() {
        return this.markMarker.getPosition();
    },

    set_mark_command: Ymacs_Interactive("d", function(x){
        this.clearTransientMark();
        this.setMark(x);
        if (this.currentCommand == "set_mark_command") {
            this.signalInfo("Mark set", null, 1000);
            this.setq("sticky_mark", true);
        }
    }),

    exchange_point_and_mark: Ymacs_Interactive("^", function(){
        this.transientMarker = this.createMarker();
        this.caretMarker.swap(this.markMarker);
        this.ensureTransientMark();
        this.setq("sticky_mark", true);
    }),

    mark_whole_buffer: Ymacs_Interactive(function(){
        this.clearTransientMark();
        this.cmd("end_of_buffer");
        this.ensureTransientMark();
        this.cmd("beginning_of_buffer");
        this.ensureTransientMark();
    }),

    recenter_top_bottom: Ymacs_Interactive("^", function() {
        this.whenActiveFrame(function(frame){
            frame.recenterTopBottom(this.sameCommandCount() % 3);
        });
    }),

    recenter: Ymacs_Interactive("^", function() {
        this.whenActiveFrame(function(frame){
            frame.centerOnCaret();
        });
    }),

    ensure_caret_visible: Ymacs_Interactive(function() {
        this.whenActiveFrame(function(frame){
            if (frame.ensureCaretVisible())
                frame.centerOnCaret();
        });
    }),

    /* -----[ paragraphs ]----- */

    get_fill_paragraph_region: function() {
        let r1 = this.cmd("save_excursion", () => {
            if (!this.cmd("looking_at", this.getq("syntax_paragraph_sep")))
                this.cmd("forward_paragraph");
            let end = this.point() - 1;
            this.cmd("backward_paragraph");
            let begin = this.point();
            return { begin, end };
        });
        let r2 = this.cmd("base_limit_fill_paragraph_region")
            || this.cmd("xml_limit_fill_paragraph_region");
        if (r2) {
            return { begin: Math.max(r1.begin, r2.begin),
                     end: Math.min(r1.end, r2.end) };
        }
        return r1;
    },

    fill_paragraph: Ymacs_Interactive("^rP", function(begin, end, noPrefix) {
        this.cmd("save_excursion", function(){
            if (!this.transientMarker) {
                let r = this.cmd("get_fill_paragraph_region");
                begin = r.begin;
                end = r.end;
            } else {
                this.clearTransientMark();
            }

            this.cmd("goto_char", begin);
            var eop = this.createMarker(end);

            // identify the prefix to use for each line
            var prefix = "", del = /\s+/y;
            if (this.cmd("looking_at", /\s*\/\/+\s*/y)) {
                prefix = this.matchData[0];
                del = /\s*\/\/+\s*/y;
            }
            else if (this.cmd("looking_at", /\s*\/\*\s*/y)) {
                prefix = " ".repeat(this.matchData[0].length);
                del = /\s*\**\s*/y;
            }
            else if (this.cmd("looking_at", /\s*[#>;\s]+\s*/y)) {
                prefix = this.matchData[0];
                del = /\s*[#>;\s]*\s*/y;
            }
            else if (this.cmd("looking_at", /\s*([-*]|[0-9]+\.|\(?[a-z][\).])?\s+/iy)) {
                prefix = " ".repeat(this.matchData[0].length);
                del = /\s*[#>;\s]*\s*/y;
            }

            if (noPrefix) {
                this._deleteText(this.point(), this.point() + this.matchData[0].length);
                prefix = "";
            }

            // remove newlines first
            while (true) {
                this.cmd("end_of_line");
                this.cmd("backward_delete_whitespace");
                if (this.point() >= eop.getPosition())
                    break;
                this._replaceText(this.point(), this.point() + 1, " ");
                if (del && this.cmd("looking_at", del)) {
                    this._deleteText(this.point(), this.point() + this.matchData[0].length);
                }
            }

            this.cmd("beginning_of_line");

            // main operation
            var bol = this.point(), done = false;
            while (!done) {
                var p = this.point();
                if (!this.cmd("search_forward_regexp", /\s/g)) {
                    done = true;
                }
                if (this.point() >= eop.getPosition()) {
                    this.cmd("goto_char", eop);
                    done = true;
                }
                if (this._rowcol.col > this.getq("fill_column") + 1) {
                    if (p > bol) {
                        this.cmd("goto_char", p);
                    }
                    this.cmd("backward_delete_whitespace");
                    this.cmd("newline");
                    this.cmd("insert", prefix);
                    bol = this.point();
                }
            }

            eop.destroy();
        });
    }),

    // this looks at the style of the current paragraph and starts
    // a similar one, i.e. using same indentation level and prefix
    // (list-like prefixes are incremented)
    start_next_paragraph: Ymacs_Interactive(function() {
        this.cmd("backward_paragraph");

        // identify the prefix to use for each line
        var prefix = "";
        if (this.cmd("looking_at", /(\s*)([0-9]+)(\.\s+)/y)) {
            prefix = this.matchData[1] +
                (parseInt(this.matchData[2], 10) + 1) +
                this.matchData[3];
        }
        else if (this.cmd("looking_at", /(\s*\(?)([a-z])([\.\)]\s+)/iy)) {
            prefix = this.matchData[1] +
                String.fromCharCode(this.matchData[2].charCodeAt(0) + 1) +
                this.matchData[3];
        }
        else if (this.cmd("looking_at", /\s*[#>;*\s-]+\s*/y)) {
            prefix = this.matchData[0];
        }

        this.cmd("forward_paragraph");
        if (this.cmd("eob_p"))
            this.cmd("newline");

        this.cmd("insert", "\n", prefix);

        if (!this.cmd("looking_at", /\n\n/y)) {
            this.cmd("newline");
            this.cmd("backward_char");
        };
    }),

    scroll_down_half: Ymacs_Interactive_X(function() {
        this.whenActiveFrame(function(frame){
            var hl = frame.heightInLines();
            this.cmd("forward_line", Math.round(hl / 1.33));
            this.cmd("recenter");
        });
    }),

    scroll_up_half: Ymacs_Interactive_X(function() {
        this.whenActiveFrame(function(frame){
            var hl = frame.heightInLines();
            this.cmd("backward_line", Math.round(hl / 1.33));
            this.cmd("recenter");
        });
    }),

    scroll_up: Ymacs_Interactive("p", function(arg){
        if (arg == null) arg = 3;
        this.whenActiveFrame(function(frame){
            frame.scrollUp(arg);
        });
    }),

    scroll_down: Ymacs_Interactive("p", function(arg){
        if (arg == null) arg = 3;
        this.whenActiveFrame(function(frame){
            frame.scrollDown(arg);
        });
    }),

    nuke_trailing_whitespace: Ymacs_Interactive(function() {
        this.cmd("save_excursion", function(){
            this.cmd("goto_char", 0);
            while (this._rowcol.row < this.code.length) {
                var line = this.code[this._rowcol.row],
                m = /\s+$/.exec(line);
                if (m) {
                    this.cmd("beginning_of_line");
                    this._deleteText(this.point() + m.index, this.point() + line.length);
                }
                if (!this.cmd("forward_line"))
                    break;
            }
        });
    }),

    match_string: function(n) {
        return this.matchData[n];
    },

    match_beginning: function() {
        return this.matchData.index;
    },

    match_end: function() {
        return this.matchData.index + this.matchData[0].length;
    },

    undo: Ymacs_Interactive_X(function() {
        this._placeUndoBoundary();
        if (!this._playbackUndo()) {
            this.signalError("No further undo information");
        }
    }),

    goto_last_change: Ymacs_Interactive(function() {
        let a = [], pos = this.point();
        this.__undoQueue.forEach(x => {
            if (x.type == 3) {
                let m = x.markers.find(m => m[0] === this.caretMarker);
                if (m) a.push(m[1]);
            }
        });
        if (a.length) {
            a = a.reverse();
            let i = this.sameCommandCount() % a.length;
            this.cmd("goto_char", a[i]);
        }
    }),

    center_line: Ymacs_Interactive("p", function(n){
        if (n == null) n = 1;
        for (let i = 0; i < n; ++i) {
            if (i > 0) this.cmd("forward_line");
            this.cmd("save_excursion", function(){
                this.cmd("end_of_line");
                this.cmd("backward_delete_whitespace", true);
                this.cmd("beginning_of_line");
                this.cmd("delete_whitespace", true);
                var line = this.code[this._rowcol.row];
                var indent = Math.floor((this.getq("fill_column") - line.length) / 2);
                this.cmd("insert", " ".repeat(indent));
            });
        }
    }),

    /* -----[ dabbrev ]----- */

    dabbrev_expand: Ymacs_Interactive_X(function() {
        if (this.previousCommand != "dabbrev_expand")
            this.setq("dabbrev_context", null);

        var ctx = this.getq("dabbrev_context");
        if (!ctx) {
            ctx = this.setq("dabbrev_context", {});
            var p1 = this.cmd("save_excursion", function(){
                this.cmd("bind_variables", {
                    syntax_word: this.getq("syntax_word_dabbrev")
                }, "backward_word");
                return this.point();
            });
            if (p1 == this.point())
                return this.signalError("Nothing to expand");
            ctx.search = this.cmd("buffer_substring", p1, this.point());
            ctx.point = p1;
            ctx.length = this.point() - p1;
            ctx.lastSearch = p1;
            ctx.encountered = Object.create(null);
            ctx.forward = false;
            ctx.buffer = this;
            ctx.seenBuffers = [ this ];
            ctx.startBuffer = this;
        }
        var expansion;

        // in the following excursion, *this* is ctx.buffer,
        // not necessarily the currently active buffer.  It's
        // purpose is to determine the next expansion and
        // setup the context so that the next invocation would
        // continue.
        ctx.buffer.cmd("save_excursion", function repeat(){
            var word = this.getq("syntax_word_dabbrev");
            var p1;
            var found = false;
            this.cmd("goto_char", ctx.lastSearch);
            // console.log("last at: %d", ctx.lastSearch);
            if (!ctx.forward) {
                while (this.cmd("search_backward", ctx.search)) {
                    if (!word.test(this.charAt(-1))) {
                        found = true;
                        break;
                    }
                }
                if (found) {
                    p1 = this.point();
                    ctx.lastSearch = p1;
                    this.cmd("goto_char", p1 + ctx.search.length);
                } else {
                    ctx.forward = true;
                    ctx.lastSearch = ctx.point + ctx.length;
                    repeat.call(this);
                    return;
                }
            } else {
                while (this.cmd("search_forward", ctx.search))
                    if (!word.test(this.charAt(-ctx.search.length - 1))) {
                        found = true;
                        break;
                    }
                if (found) {
                    ctx.lastSearch = this.point();
                    p1 = this.point() - ctx.search.length;
                } else {
                    ctx.buffer = this.whenYmacs("getNextBuffer", this);
                    if (ctx.seenBuffers.includes(ctx.buffer)) {
                        expansion = ctx.search;
                        ctx.startBuffer.signalError("No more completions");
                        ctx.lastSearch = ctx.point + ctx.length;
                        ctx.startBuffer.setq("dabbrev_context", null);
                        return;
                    } else {
                        ctx.seenBuffers.push(ctx.buffer);
                        ctx.lastSearch = 0;
                        ctx.buffer.cmd("save_excursion", repeat);
                        return;
                    }
                }
            }
            if (p1 != null) {
                // console.log("%s at %d, next from %d", ctx.search, p1, ctx.lastSearch);
                this.cmd("bind_variables", {
                    syntax_word: this.getq("syntax_word_dabbrev")
                }, "forward_word");
                expansion = this.cmd("buffer_substring", p1, this.point());
                if (ctx.encountered[expansion])
                    repeat.call(this);
            }
        });
        if (expansion != null) {
            this._replaceText(ctx.point, ctx.point + ctx.length, expansion);
            ctx.length = expansion.length;
            ctx.encountered[expansion] = true;
        }
    }),

    /* -----[ frames and buffers ]----- */

    split_frame_vertically: Ymacs_Interactive("p", function(percent) {
        if (percent == null) percent = "50%";
        else percent += "%";
        this.whenActiveFrame("vsplit", percent);
    }),

    split_frame_horizontally: Ymacs_Interactive("p", function(percent) {
        if (percent == null) percent = "50%";
        else percent += "%";
        this.whenActiveFrame("hsplit", percent);
    }),

    delete_other_frames: Ymacs_Interactive(function() {
        this.whenActiveFrame("deleteOtherFrames");
    }),

    delete_frame: Ymacs_Interactive(function() {
        this.whenActiveFrame("deleteFrame");
    }),

    other_frame: Ymacs_Interactive(function() {
        this.whenYmacs("focusOtherFrame");
    }),

    windmove: function(dir) {
        this.whenYmacs(function(ymacs){
            var f = ymacs.getFrameInDirection(dir);
            if (f) f.focus();
        });
    },

    next_buffer: Ymacs_Interactive(function() {
        this.whenYmacs("switchToNextBuffer", this.sameCommandCount() + 1);
    }),

    previous_buffer: Ymacs_Interactive(function() {
        this.whenYmacs("switchToPreviousBuffer", this.sameCommandCount() + 1);
    }),

    switch_to_buffer: Ymacs_Interactive("BSwitch to buffer: ", function(buffer) {
        this.whenYmacs(function(ymacs){
            if (!/\S/.test(buffer)) {
                if (ymacs.buffers.length < 2) return;
                buffer = ymacs.buffers[1];
            }
            ymacs.switchToBuffer(buffer);
        });
    }),

    kill_buffer: Ymacs_Interactive(function() {
        var self = this;
        function kill() {
            self.whenYmacs(function(ymacs){
                ymacs.killBuffer(self);
            });
        }
        if (self.dirty()) {
            var msg = "Buffer " + self.name + " modified; kill anyway?";
            self.cmd("minibuffer_yn", msg, function(yes){
                if (yes) kill();
            });
        } else {
            kill();
        }
    }),

    rename_buffer: Ymacs_Interactive("sRename current buffer to: ", function(name){
        this.whenYmacs(function(ymacs){
            ymacs.renameBuffer(this, name);
        });
    }),

    /* -----[ other ]----- */

    delete_region_or_line: Ymacs_Interactive("^", function() {
        if (!this.deleteTransientRegion()) {
            this.cmd("beginning_of_line");
            var pos = this.point();
            this.cmd("end_of_line");
            this.cmd("forward_char");
            if (this.point() != pos) {
                this._deleteText(pos, this.point());
                return true;
            }
        }
    }),

    // http://mihai.bazon.net/blog/close-last-xml-tag-emacs
    close_last_xml_tag: Ymacs_Interactive_X(function() {
        var tag, quote;
        this.cmd("save_excursion", function() {
            var skip = 1;
            while (skip != 0 && this.cmd("search_backward_regexp", /<\x2f?([a-zA-Z0-9:_-]+)/g)) {
                tag = this.cmd("match_string", 1);
                if (this.cmd("looking_at", /<\x2f/y)) {
                    ++skip;
                }
                else if (!this.cmd("looking_at", /<[^\x2f][^>]*?\x2f>/y)) {
                    --skip;
                }
            }
            if (skip != 0)
                tag = null;
        });
        if (tag) {
            this.cmd("insert", "</", tag, ">");
        } else {
            throw new Ymacs_Exception("Couldn't find a tag to close");
        }
    }),

    bind_variables: function() {
        return this.withVariables.apply(this, arguments);
    },

    for_region: Ymacs_Interactive("^r\nCExecute command within region: ", function(begin, end, func) {
        if (end < begin) { var tmp = begin; begin = end; end = tmp; } // MACROS!  I WANT MACROS!  EVAL SUCKS. x-(
        if (!(func instanceof Function))
            func = this.COMMANDS[func];
        this.clearTransientMark();
        this.cmd("goto_char", begin);
        begin = this.createMarker(begin, true);
        end = this.createMarker(end);
        this.withCommands(
            {
                goto_char: function(pos){
                    if (pos >= begin.getPosition() && pos <= end.getPosition())
                        return this._repositionCaret(pos);
                    throw "YMACS_RESTRICT";
                }
            },
            function() {
                try {
                    while (true) {
                        var tmp = this.point();
                        func.call(this);
                        if (this.point() == tmp && !this.cmd("forward_line"))
                            break;
                    }
                } catch(ex) {
                    if (ex !== "YMACS_RESTRICT")
                        throw ex;
                } finally {
                    begin.destroy();
                    end.destroy();
                }
            }
        );
    }),

    comment_region: Ymacs_Interactive("^r", function(begin, end){
        var cmmt = this.getq("syntax_comment_line") || this.getq("syntax_comment_multi");
        if (!cmmt) return;
        this.clearTransientMark();
        this.cmd("save_excursion", function(){
            end = this.createMarker(end);
            this.cmd("goto_char", begin);
            var min = 100000;
            out: while (this.point() < end.getPosition()) {
                while (this.cmd("looking_at", /\s*$/my)) {
                    if (!this.cmd("forward_line")) break out;
                }
                var col = this._rowcol.col;
                while (this.cmd("looking_at", /\s/y) && col < min) {
                    if (!this.cmd("forward_char")) break out;
                    ++col;
                }
                if (col < min) min = col;
                if (Array.isArray(cmmt.ch)) {
                    this.cmd("insert", cmmt.ch[0], " ");
                    this.cmd("end_of_line");
                    this.cmd("insert", " ", cmmt.ch[1]);
                } else {
                    this.cmd("insert", cmmt.ch, " ");
                }
                this.cmd("beginning_of_line");
                if (!this.cmd("forward_line")) break out;
            }
            this.cmd("goto_char", end);
            if (this._rowcol.col > 0 && !this.cmd("looking_at", /\s*$/my))
                this.cmd("newline_and_indent");
        });
    }),

    uncomment_region: Ymacs_Interactive("r", function(begin, end){
        var cmmt1 = this.getq("syntax_comment_line");
        var cmmt2 = this.getq("syntax_comment_multi");
        if (!cmmt1 && !cmmt2) return;
        this.clearTransientMark();
        this.cmd("save_excursion", function(){
            end = this.createMarker(end);
            this.cmd("goto_char", begin);
            while (this.point() < end.getPosition()) {
                this.cmd("forward_whitespace");
                if (cmmt1 && this.cmd("looking_at", cmmt1.rx)) {
                    this.cmd("delete_char", this.matchData[0].length);
                } else if (cmmt2 && this.cmd("looking_at", cmmt2.rx)) {
                    this._replaceText(this.point(), this.point() + this.matchData[0].length, this.matchData[1]);
                }
                this.cmd("beginning_of_line");
                if (!this.cmd("forward_line")) break;
            }
        });
    }),

    comment_dwim: Ymacs_Interactive("^r", function(begin, end){
        var cmmt1 = this.getq("syntax_comment_line");
        var cmmt2 = this.getq("syntax_comment_multi");
        if (!cmmt1 && !cmmt2) return;
        if (this.transientMarker) {
            this.cmd("save_excursion", function(){
                this.cmd("goto_char", begin);
                var already_comment =
                    (cmmt1 && this.cmd("looking_at", cmmt1.rx))
                    ||
                    (cmmt2 && this.cmd("looking_at", cmmt2.rx));
                if (already_comment) {
                    this.cmd("uncomment_region", begin, end);
                } else {
                    this.cmd("comment_region", begin, end);
                }
            });
        } else {
            this.cmd("end_of_line");
            if (cmmt1) {
                this.cmd("insert", " ", cmmt1.ch, " ");
            } else {
                this.cmd("insert", " ", cmmt2.ch[0], cmmt2.ch[1]);
                this.cmd("backward_char", cmmt2.ch[1].length);
            }
            this.cmd("indent_line");
        }
    }),

    what_cursor_position: Ymacs_Interactive("^", function(){
        var ch = this.charAt(), chname = ch;
        if (ch == null)
            chname = "EOF";
        else if (ch == " ")
            chname = "Space";
        else if (ch == "\n")
            chname = "Newline";
        this.popupMessage({
            isHtml: true,
            atCaret: true,
            text: TMPL_CHAR_INFO({
                ch      : DOM.htmlEscape(chname),
                code    : ch ? ch.charCodeAt(0) : null,
                codeHex : ch ? ch.charCodeAt().toString(16).toUpperCase() : null,
                point   : this.point(),
                size    : this.getCodeSize(),
                sizeKB  : formatBytes(this.getCodeSize(), 2)
            }),
        });
    }),

    set_fill_column: Ymacs_Interactive("p", function(value){
        let next = value => {
            let prev = this.getq("fill_column");
            this.setq("fill_column", +value);
            this.signalInfo(`Fill column set to ${value} (was ${prev})`, false, 5000);
        };
        if (value == null) {
            this.whenMinibuffer(mb => {
                this.cmd("minibuffer_prompt", "Set fill column to: ");
                mb.setMark();
                mb.transientMarker = mb.createMarker(mb.point(), true);
                mb.cmd("insert", String(this._rowcol.col));
                mb.ensureTransientMark();
                this.cmd("minibuffer_read_number", next);
            });
        } else {
            next(value);
        }
    }),

    bury_buffer: Ymacs_Interactive(function(){
        let ymacs = this.ymacs;
        ymacs.switchToNextBuffer();
    }),

    describe_mode: Ymacs_Interactive(function(){
        let ymacs = this.ymacs;
        let buf = ymacs.switchToBuffer("*Help*");
        ymacs.switchToBuffer(buf);
        buf.pushKeymap(help_keymap);
        buf._disableUndo(() => {
            buf.setCode(`Active keymaps in buffer "${this.name}".\nPress \`q\` to go back.\n\n`);
            buf.cmd("end_of_buffer");
            let dumpKeymap = (prefix, definitions) => {
                for (let [ key, val ] of Object.entries(definitions)) {
                    let def = (prefix ? prefix + " " : "") + key;
                    if (Array.isArray(val)) {
                        let txt = "  ";
                        txt += def;
                        for (let n = 20 - def.length; n-- >= 0;) txt += " ";
                        txt += " : " + (
                            typeof val[0] == "function" ? "(lambda)" : JSON.stringify(val)
                        );
                        buf.cmd("insert", txt);
                        buf.cmd("newline");
                    } else if (val && typeof val == "object") {
                        dumpKeymap(def, val);
                    }
                }
            };
            [...this.keymap].reverse().forEach((keymap, index) => {
                if (index > 0) buf.cmd("insert", "\n");
                buf.cmd("insert", `Keymap: ${keymap.name || "(unnamed)"}\n`);
                buf.cmd("newline");
                dumpKeymap("", keymap.definitions);
            });
        });
        buf.dirty(false);
        buf.cmd("goto_char", 0);
    }),

    window_configuration_to_register: Ymacs_Interactive(function(){
        this.whenMinibuffer(mb => {
            mb.prompt("Window configuration to register:");
            this.setq("read_register_callback", (reg) => {
                let ymacs = this.ymacs;
                ymacs.registers[reg] = { frames: ymacs.getFrameConfig() };
                this.signalInfo(`Saved to register "${reg}"`, null, 2000);
            });
            this.pushKeymap(read_register_keymap);
        });
    }),

    jump_to_register: Ymacs_Interactive(function(){
        this.whenMinibuffer(mb => {
            mb.prompt("Jump to register:");
            this.setq("read_register_callback", (reg) => this.ymacs.jumpToRegister(reg));
            this.pushKeymap(read_register_keymap);
        });
    }),

});

/* -----[ rectangle functions (vertical editing) ]----- */

(function(){

    function apply_on_rectangle(buffer, begin, end, func) {
        buffer.cmd("save_excursion", function(){
            var p1 = this._positionToRowCol(begin),
            p2 = this._positionToRowCol(end),
            width = Math.abs(p2.col - p1.col);
            for (var line = p1.row; line <= p2.row; ++line) {
                this.cmd("goto_char", this._rowColToPosition(line, 0));
                var text = this.code[line],
                c1 = p1.col,
                c2 = p2.col,
                p = this.point(), ws = 0;
                if (c1 > c2) {
                    var tmp = c1;
                    c1 = c2;
                    c2 = tmp;
                }
                if (c1 > text.length) {
                    ws = c1 - text.length;
                    c1 = text.length;
                }
                if (c2 > text.length) {
                    c2 = text.length;
                }
                func.call(this, p + c1, p + c2, ws, width);
            }
        }, begin == buffer.point());
    };

    Ymacs_Buffer.newCommands({

        string_rectangle: Ymacs_Interactive("r\nsString rectangle: ", function(begin, end, string) {
            apply_on_rectangle(this, begin, end, function(c1, c2, ws){
                if (ws > 0) {
                    this._insertText(" ".repeat(ws), c1);
                } else {
                    this._deleteText(c1, c2);
                }
                this._insertText(string, c1 + ws);
            });
        }),

        kill_rectangle: Ymacs_Interactive("r", function(begin, end){
            var text = [];
            apply_on_rectangle(this, begin, end, function(c1, c2, ws, width){
                var str = this._bufferSubstring(c1, c2);
                if (c2 - c1 < width)
                    str += " ".repeat(width - c2 + c1);
                text.push(str);
                this._deleteText(c1, c2);
            });
            this.setq("killed_rectangle", text);
        }),

        clear_rectangle: Ymacs_Interactive("r", function(begin, end){
            this.cmd("string_rectangle", begin, end,
                     " ".repeat(Math.abs(this._positionToRowCol(end).col -
                                         this._positionToRowCol(begin).col)));
        }),

        insert_rectangle: function(point, rect) {
            var col = this._positionToRowCol(point).col;
            this.setMark(point);
            rect.forEach((text, i) => {
                if (i > 0) {
                    if (!this.cmd("forward_line")) {
                        this.cmd("end_of_line");
                        this.cmd("newline");
                    }
                    this.cmd("move_to_column", col, true);
                }
                this.cmd("insert", text);
            });
        },

        yank_rectangle: Ymacs_Interactive("d", function(point){
            var kr = this.getq("killed_rectangle");
            if (kr == null)
                throw new Ymacs_Exception("No killed rectangle");
            this.cmd("insert_rectangle", point, kr);
        }),

        _next_is_meta: function() {
            if (!this.__nextIsMeta) {
                this.__nextIsMeta = true;
                this.currentKeys.pop();
            }
        },

    });

})();

(function(){
    Ymacs_Buffer.newCommands({
        kmacro_start_macro: Ymacs_Interactive("p", function(arg) {
            if (this.ymacs.isRunningMacro()) {
                return;
            }
            if (this.ymacs.isRecordingMacro()) {
                this.signalError("Already defining keyboard macro.");
                return;
            }
            this.signalInfo("Defining keyboard macro");
            this.ymacs.startMacro(arg !== null);
        }),
        kmacro_end_macro: Ymacs_Interactive(function() {
            if (this.ymacs.isRunningMacro()) {
                return;
            }
            if (!this.ymacs.isRecordingMacro()) {
                this.signalInfo("Not defining kbd macro");
                return;
            }
            this.signalInfo("Keyboard macro defined");
            this.ymacs.stopMacro();
        }),
        kmacro_end_and_call_macro: Ymacs_Interactive("p", function(arg) {
            this.ymacs.stopMacro();
            if (arg === null)
                arg = 1;
            var macro = this.ymacs.getLastMacro();
            this.interactiveEvent(null);
            this.ymacs.runMacro(arg, macro);
        })
    });
})();

/* -----[ transient mark extension commands ]----- */

[
    "forward_char",
    "forward_word",
    "forward_line",
    "forward_paragraph",
    "forward_sexp",
    "beginning_of_line",
    "beginning_of_indentation_or_line",
    "beginning_of_buffer",
    "backward_char",
    "backward_word",
    "backward_line",
    "backward_paragraph",
    "backward_sexp",
    "end_of_line",
    "end_of_buffer"

].forEach(function(cmd) {
    Ymacs_Buffer.COMMANDS[cmd + "_mark"] = Ymacs_Interactive("^", function(){
        this.ensureTransientMark();
        this.cmdApply(cmd, arguments);
        this.ensureTransientMark();
    });
});
