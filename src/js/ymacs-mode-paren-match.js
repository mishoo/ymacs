/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT

import { delayed } from "./ymacs-utils.js";
import { Ymacs_Keymap } from "./ymacs-keymap.js";
import { Ymacs_Buffer } from "./ymacs-buffer.js";
import { Ymacs_Exception } from "./ymacs-exception.js";
import { Ymacs_Interactive } from "./ymacs-interactive.js";
import { compareRowCol, caretInside } from "./ymacs-tokenizer.js";

let Ymacs_Keymap_ParenMatch = Ymacs_Keymap.define("parenmatch", {
    "C-M-x"                        : "goto_matching_paren",
    "C-M-q"                        : "indent_sexp",
    "C-M-f && C-M-n"               : "forward_sexp",
    "C-M-b && C-M-p"               : "backward_sexp",
    "C-M-u && M-a && C-M-ArrowUp"  : "backward_up_list",
    "C-M-a"                        : "beginning_of_defun",
    "C-M-e"                        : "end_of_defun",
    "M-e"                          : "up_list",
    "C-M-ArrowDown"                : "down_list",
    "M-C-k"                        : "kill_sexp",
    "M-C-Space"                    : "mark_sexp",
    "M-C-t"                        : "transpose_sexps",

    "("                            : [ "paredit_open_pair", "(", ")" ],
    "["                            : [ "paredit_open_pair", "[", "]" ],
    "{"                            : [ "paredit_open_pair", "{", "}" ],
    "❰"                            : [ "paredit_open_pair", "❰", "❱" ],
    "«"                            : [ "paredit_open_pair", "«", "»" ],
    "“"                            : [ "paredit_open_pair", "“", "”" ],
    '"'                            : [ "paredit_open_pair", '"', '"', /[\"\\]/g ],

    ")"                            : [ "paredit_close_pair", "(", ")" ],
    "]"                            : [ "paredit_close_pair", "[", "]" ],
    "}"                            : [ "paredit_close_pair", "{", "}" ],
    "❱"                            : [ "paredit_close_pair", "❰", "❱" ],
    "»"                            : [ "paredit_close_pair", "«", "»" ],
    "”"                            : [ "paredit_close_pair", "“", "”" ],

    "M-("                          : [ "paredit_wrap_round", "(", ")" ],
    "M-["                          : [ "paredit_wrap_round", "[", "]" ],
    "M-{"                          : [ "paredit_wrap_round", "{", "}" ],
    "M-❰"                          : [ "paredit_wrap_round", "❰", "❱" ],
    "M-«"                          : [ "paredit_wrap_round", "«", "»" ],
    "M-“"                          : [ "paredit_wrap_round", "“", "”" ],
    'M-"'                          : [ "paredit_wrap_round", '"', '"', /[\"\\]/g ],

    "M-r"                          : "paredit_raise_sexp",
    "M-s"                          : "paredit_splice_sexp",
    "Backspace"                    : "paredit_backward_delete_char",
    //"Delete && C-d"                : "paredit_delete_char",
    "Enter"                        : "paredit_newline_and_indent",
    "; && : && , && ."             : "paredit_electric_char",
});

var PARENS = {
    "(" : ")",
    "[" : "]",
    "{" : "}",
    "❰" : "❱",
    "«" : "»",
    "“" : "”",
    '"' : '"',
    "'" : "'",
    '`' : '`',
};

var R_PARENS = {
    ")" : "(",
    "]" : "[",
    "}" : "{",
    "❱" : "❰",
    "»" : "«",
    "”" : "“",
    '"' : '"',
    "'" : "'",
    '`' : '`',
};

function ERROR(o) {
    throw new Ymacs_Exception("Balanced expression not found");
};

function startOf(paren) {
    return paren.c1 ?? paren.col;
}

function endOf(paren) {
    return paren.c2 ?? (paren.col + 1);
}

function typeOf(paren) {
    if (paren.opened) return paren.opened.type || "";
    return paren.type || "";
}

function touches(paren, caret) {
    return paren.line == caret.row
        && startOf(paren) <= caret.col
        && caret.col <= endOf(paren);
}

Ymacs_Buffer.newCommands({

    get_paren_at_point() {
        let rc = this._rowcol;
        let parens = this.tokenizer.getPP();
        let a = [];
        for (let op of parens) {
            if (!op.type) continue;
            let cp = op.closed;
            if (!cp) continue;
            if (touches(op, rc)) a.push(op);
            if (touches(cp, rc)) a.push(cp);
        }
        if (!a.length)
            return null;
        if (a.length == 1)
            return a[0].opened || a[0];
        if (/\w/.test(typeOf(a[0])) && !/\w/.test(typeOf(a[1])))
            return a[0].opened || a[0];
        if (/\w/.test(typeOf(a[1])) && !/\w/.test(typeOf(a[0])))
            return a[1].opened || a[1];
        a = a.sort(compareRowCol);
        a = a[0].opened ? a[0] : a[1];
        return a.opened || a;
    },

    matching_paren() {
        this.tokenizer.finishParsing();
        let rc = this._rowcol;
        let p = this.cmd("get_paren_at_point");
        if (p) {
            if (touches(p, rc))
                return this._rowColToPosition(p.closed.line, endOf(p.closed));
            return this._rowColToPosition(p.line, startOf(p));
        }
    },

    indent_sexp: Ymacs_Interactive(function() {
        var pos = this.cmd("matching_paren");
        if (pos != null) {
            this.cmd("indent_region", this.point(), pos);
        } else {
            ERROR(this);
        }
    }),

    goto_matching_paren: Ymacs_Interactive(function() {
        var pos = this.cmd("matching_paren");
        if (pos != null) {
            this.cmd("goto_char", pos);
            return true;
        }
    }),

    forward_sexp: Ymacs_Interactive(function() {
        this.tokenizer.finishParsing();
        let next;
        let rc = this._rowcol;
        let parens = this.tokenizer.getPP();
        for (let i = 0; i < parens.length; ++i) {
            let p = parens[i];
            if (p.line > rc.row || (p.line == rc.row && startOf(p) >= rc.col)) {
                next = p;
                break;
            }
        }
        this.withVariables({
            syntax_word: this.getq("syntax_word_sexp"),
        }, "forward_word");
        if (next && next.closed && compareRowCol(this._rowcol, next) > 0) {
            this.cmd("goto_char", this._rowColToPosition(next.closed.line, endOf(next.closed)));
        }
    }),

    backward_sexp: Ymacs_Interactive(function() {
        this.tokenizer.finishParsing();
        let prev;
        let rc = this._rowcol;
        let parens = this.tokenizer.getPP().filter(p => p.closed).map(p => p.closed).sort(compareRowCol);
        for (let i = parens.length; --i >= 0;) {
            let p = parens[i];
            if (p.line < rc.row
                || (p.line == rc.row
                    && (startOf(p) < rc.col
                        || (p.c1 == p.c2 && startOf(p) == rc.col)))) {
                prev = p;
                break;
            }
        }
        this.withVariables({
            syntax_word: this.getq("syntax_word_sexp"),
        }, "backward_word");
        if (prev && prev.opened && compareRowCol(this._rowcol, { line: prev.line, col: endOf(prev) }) < 0) {
            this.cmd("goto_char", this._rowColToPosition(prev.opened.line, startOf(prev.opened)));
        }
    }),

    mark_sexp: Ymacs_Interactive("^r", function(begin, end){
        this.tokenizer.finishParsing();
        let paren = this.cmd("get_paren_at_point");
        if (paren?.outer) {
            this.cmd("goto_char", this._rowColToPosition(paren.outer.l1, paren.outer.c1));
            this.ensureTransientMark();
            this.cmd("goto_char", this._rowColToPosition(paren.outer.l2, paren.outer.c2));
            this.setMark(this.point());
            this.transientMarker.swap(this.caretMarker);
        }
        else this.cmd("save_excursion", function(){
            if (this.transientMarker)
                this.cmd("goto_char", end);
            this.ensureTransientMark();
            this.cmd("forward_sexp");
            this.setMark(this.point());
            this.transientMarker.swap(this.caretMarker);
        });
        this.ensureTransientMark();
        this.setq("sticky_mark", true);
    }),

    kill_sexp: Ymacs_Interactive(function() {
        this._killingAction(
            this.point(),
            this.cmd("save_excursion", function() {
                this.cmd("forward_sexp");
                return this.point();
            })
        );
    }),

    transpose_sexps: Ymacs_Interactive(function() {
        var a = [];
        this.cmd("forward_sexp"); a.push(this.point());
        this.cmd("backward_sexp"); a.push(this.point());
        this.cmd("backward_sexp"); a.push(this.point());
        this.cmd("forward_sexp"); a.push(this.point());
        this.cmd("goto_char", this._swapAreas(a));
    }),

    down_list: Ymacs_Interactive(function(){
        this.tokenizer.finishParsing();
        let rc = this._rowcol;
        let lc = { line: rc.row, col: rc.col };
        let p = this.tokenizer.getPP().filter(p => p.closed).find(p => compareRowCol(p, lc) >= 0);
        if (p != null) {
            this.cmd("goto_char", this._rowColToPosition(p.line, endOf(p)));
        } else {
            ERROR(this);
        }
    }),

    backward_up_list: Ymacs_Interactive(function(){
        this.tokenizer.finishParsing();
        let rc = this._rowcol;
        let p = this.tokenizer.getPP().filter(caretInside(rc)).at(-1);
        if (p != null) {
            this.cmd("goto_char", this._rowColToPosition(p.line, startOf(p)));
        } else {
            ERROR(this);
        }
    }),

    up_list: Ymacs_Interactive(function(){
        this.cmd("backward_up_list");
        this.cmd("forward_sexp");
    }),

    beginning_of_defun: Ymacs_Interactive(function(){
        this.tokenizer.finishParsing();
        let rc = this._rowcol;
        let p = this.tokenizer.getPP().filter(caretInside(rc))[0];
        if (p != null) {
            this.cmd("goto_char", this._rowColToPosition(p.line, startOf(p)));
        } else {
            this.cmd("backward_sexp");
        }
        this.cmd("back_to_indentation");
    }),

    end_of_defun: Ymacs_Interactive(function(){
        this.tokenizer.finishParsing();
        let rc = this._rowcol;
        let p = this.tokenizer.getPP().filter(caretInside(rc))[0];
        if (p != null) {
            p = p.closed;
            this.cmd("goto_char", this._rowColToPosition(p.line, endOf(p)));
        } else {
            this.cmd("forward_sexp");
        }
    }),

    paredit_raise_sexp: Ymacs_Interactive(function(){
        this.tokenizer.finishParsing();
        let p = this.tokenizer.getPP()
            .filter(caretInside(this._rowcol, "inner")).at(-1);
        if (!p) {
            this.signalError("No containing expression");
            return;
        }
        this.cmd("forward_sexp");
        this.cmd("backward_sexp");
        var start = this.point();
        this.cmd("forward_sexp");
        var end = this.point();
        this.cmd("backward_up_list");
        var kstart = this.point();
        this.cmd("forward_sexp");
        var kend = this.point();
        var sexp = this.cmd("buffer_substring", start, end);
        this._replaceText(kstart, kend, sexp);
        this.cmd("goto_char", kstart);
        this.cmd("indent_region", kstart, kstart + sexp.length);
    }),

    paredit_splice_sexp: Ymacs_Interactive(function(){
        this.tokenizer.finishParsing();
        let p = this.tokenizer.getPP()
            .filter(caretInside(this._rowcol, "inner")).at(-1);
        if (!p) {
            this.signalError("No containing expression");
            return;
        }
        if (p.inner && p.outer) {
            this.cmd("save_excursion", function(){
                let outer_begin = this._rowColToPosition(p.outer.l1, p.outer.c1);
                let outer_end = this._rowColToPosition(p.outer.l2, p.outer.c2);
                let inner_begin = this._rowColToPosition(p.inner.l1, p.inner.c1);
                let inner_end = this._rowColToPosition(p.inner.l2, p.inner.c2);
                this.withMarkers((m1, m2) => {
                    this._deleteText(inner_end, outer_end);
                    this._deleteText(outer_begin, inner_begin);
                    this.cmd("indent_region", m1, m2);
                }, inner_begin, inner_end);
            });
        } else {
            this.cmd("save_excursion", function(){
                this.cmd("backward_up_list");
                var start = this.point();
                this.cmd("forward_sexp");
                this.cmd("backward_delete_char");
                var end = this.point();
                this.cmd("goto_char", start);
                this.cmd("delete_char");
                this.cmd("indent_region", start, end - 1);
            });
        }
    }),

    paredit_backward_delete_char: Ymacs_Interactive("^p", function(n){
        if (n != null) return this.cmd("backward_delete_char", n);
        if (!this.deleteTransientRegion()) {
            if (this.cmd("looking_back", /[\(\[\{\"\❰\«\`\']/g)) {
                var close = PARENS[this.matchData[0]];
                if (close) {
                    var rx = new RegExp("\\s*\\" + close, "my");
                    if (this.cmd("looking_at", rx)) this.cmd("save_excursion", function(){
                        this.cmd("delete_whitespace");
                        this.cmd("delete_char");
                    });
                }
            }
            this.cmd("backward_delete_char");
        }
    }),

    paredit_delete_char: Ymacs_Interactive("^p", function(n){
        if (n != null) return this.cmd("delete_char", n);
        if (!this.deleteTransientRegion()) {
            if (this.cmd("looking_at", /[\]\}\)\"\❱\»\`\']/y)) {
                var open = R_PARENS[this.matchData[0]];
                if (open) {
                    var rx = new RegExp("\\" + open + "\\s*", "mg");
                    if (this.cmd("looking_back", rx)) this.cmd("save_excursion", function(){
                        this.cmd("backward_delete_whitespace");
                        this.cmd("backward_delete_char");
                    });
                }
            }
            this.cmd("delete_char");
        }
    }),

    paredit_open_pair: Ymacs_Interactive("^", function(pair_a, pair_b, backslash) {
        if (this.transientMarker) {
            this.cmd("paredit_wrap_round", pair_a, pair_b, backslash);
            return;
        }
        if (pair_a == pair_b && this.looking_at(pair_a)) {
            // presumably close; it's already there, just skip it
            this.cmd("forward_char");
        } else {
            let maybe_space = this.getq("paredit_space_before");
            if (typeof maybe_space == "function") {
                maybe_space = maybe_space.apply(this, arguments);
            }
            if (maybe_space) {
                this.cmd("insert", " ");
            }
            this.cmd("insert", pair_a);
            this.cmd("insert", pair_b);
            this.cmd("backward_char", pair_b.length);
        }
        this.cmd("paredit_maybe_indent");
    }),

    paredit_close_pair: Ymacs_Interactive("^", function(pair_a, pair_b) {
        if (this.transientMarker) {
            this.cmd("paredit_wrap_round", pair_a, pair_b);
            return;
        }
        var re = new RegExp("\\s*\\" + pair_b, "iy");
        if (this.cmd("looking_at", re))
            this._deleteText(this.point(), this.matchData.after);
        this.cmd("insert", pair_b);
        this.cmd("paredit_maybe_indent");
    }),

    paredit_wrap_round: Ymacs_Interactive("^", function(paren, closing, backslash) {
        let r = this.transientMarker
            ? this.getRegion()
            : this.cmd("save_excursion", function(){
                let begin = this.point();
                this.cmd("forward_sexp");
                return { begin: begin, end: this.point() };
            });
        let txt = this._bufferSubstring(r.begin, r.end);
        let before = this.point() < r.end;
        if (backslash) {
            txt = txt.replace(backslash, s => "\\" + s);
        }
        let m = this.createMarker(r.end);
        this.cmd("save_excursion", function(){
            this._replaceText(r.begin, r.end, paren + txt + closing);
        }, before);
        if (before) {
            this.cmd("forward_char");
        }
        this.clearTransientMark();
        this.cmd("indent_region", r.begin, m.getPosition());
        m.destroy();
    }),

    paredit_newline_and_indent: Ymacs_Interactive(function(){
        var inparens = this.looking_at(/[ \t]*([\]\}])/y)
            && this.looking_back(new RegExp("\\" + R_PARENS[this.matchData[1]] + "[ \\t]*", "g"));
        this.cmd("newline_and_indent");
        if (inparens) {
            this.cmd("newline_and_indent");
            this.cmd("backward_line");
            this.cmd("indent_line");
        }
    }),

    paredit_maybe_indent: function() {
        if (this.getq("electric_indent")) {
            this.cmd("indent_line");
        }
    },

    paredit_electric_char: Ymacs_Interactive(function(){
        this.cmd("self_insert_command");
        this.cmd("paredit_maybe_indent");
    }),

});

Ymacs_Buffer.newMode("paren_match_mode", function(){

    this.pushKeymap(Ymacs_Keymap_ParenMatch);

    var clearOvl = function() {
        this.deleteOverlay("match-paren");
    }.bind(this);

    let events = {
        beforeInteractiveCommand: function() {
            clearOvl();
        },
        afterInteractiveCommand: delayed(() => {
            let rc = this._rowcol;
            let hl = [];
            let p = this.cmd("get_paren_at_point");
            if (p) {
                hl.push({
                    line1: p.line, col1: p.c1,
                    line2: p.line, col2: p.c2
                }, {
                    line1: p.closed.line, col1: p.closed.c1,
                    line2: p.closed.line, col2: p.closed.c2
                });
            }
            this.setOverlay("match-paren", hl);
        }, 100)
    };
    this.addEventListener(events);

    if (this.getq("electric_indent") == null) {
        this.setq("electric_indent", true);
    }

    return function() {
        clearOvl();
        this.popKeymap(Ymacs_Keymap_ParenMatch);
        this.removeEventListener(events);
    };

});
