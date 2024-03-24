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

import { delayed } from "./ymacs-utils.js";
import { Ymacs_Keymap } from "./ymacs-keymap.js";
import { Ymacs_Buffer } from "./ymacs-buffer.js";
import { Ymacs_Exception } from "./ymacs-exception.js";
import { Ymacs_Interactive } from "./ymacs-interactive.js";

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
    "Delete && C-d"                : "paredit_delete_char",
    "Enter"                        : "paredit_newline_and_indent",
    "; && : && , && ."             : "paredit_electric_char",
});

function compareRowCol(p1, p2) {
    return (p1.line < p2.line)
        ? -1
        : p1.line > p2.line
        ? 1
        : (p1.col ?? p1.c1) - (p2.col ?? p2.c1);
};

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

function getPP(tok) {
    var pp = tok.context.passedParens;
    pp = pp instanceof Function ? pp() : pp;
    if (pp) pp = [...pp].sort(compareRowCol);
    return pp;
};

function caretInOP(caret, paren) {
    if (caret.row == paren.line) {
        if (paren.col != null) {
            return caret.col == paren.col;
        } else {
            return paren.c1 <= caret.col
                && caret.col <= paren.c2;
        }
    }
}

function caretInCP(caret, paren) {
    if (caret.row == paren.line) {
        if (paren.col != null) {
            return caret.col == paren.col + 1;
        } else {
            return paren.c1 <= caret.col
                && caret.col <= paren.c2;
        }
    }
}

function startOf(paren) {
    return paren.c1 ?? paren.col;
}

function endOf(paren) {
    return paren.c2 ?? paren.col + 1;
}

Ymacs_Buffer.newCommands({

    matching_paren: function() {
        let parser = this.tokenizer.getLastParser();
        if (parser) {
            let rc = this._rowcol;
            let parens = getPP(parser);
            for (let op of parens) {
                let cp = op.closed;
                if (caretInOP(rc, op)) {
                    return this._rowColToPosition(cp.line, endOf(cp));
                } else if (caretInCP(rc, cp)) {
                    return this._rowColToPosition(op.line, startOf(op));
                }
            }
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
        let parser = this.tokenizer.finishParsing();
        if (parser) {
            // find next paren
            let rc = this._rowcol;
            let parens = getPP(parser).sort(compareRowCol);
            let next = null;
            for (let i = 0; i < parens.length; ++i) {
                let p = parens[i];
                if (p.line > rc.row || (p.line == rc.row && startOf(p) >= rc.col)) {
                    next = p;
                    break;
                }
            }
            if (!next || !next.closed) {
                ERROR(this);
                return;
            }
            this.cmd("goto_char", this._rowColToPosition(next.closed.line, endOf(next.closed)));
            return true;
        }
    }),

    backward_sexp: Ymacs_Interactive(function() {
        let parser = this.tokenizer.finishParsing();
        if (parser) {
            // find next paren
            let rc = this._rowcol;
            let parens = getPP(parser).filter(p => p.closed).map(p => p.closed).sort(compareRowCol);
            let prev = null;
            for (let i = parens.length; --i >= 0;) {
                let p = parens[i];
                if (p.line < rc.row || (p.line == rc.row && startOf(p) < rc.col)) {
                    prev = p;
                    break;
                }
            }
            if (!prev) {
                ERROR(this);
                return;
            }
            this.cmd("goto_char", this._rowColToPosition(prev.opened.line, startOf(prev.opened)));
            return true;
        }
    }),

    mark_sexp: Ymacs_Interactive("^r", function(begin, end){
        this.cmd("save_excursion", function(){
            if (this.transientMarker)
                this.cmd("goto_char", end);
            this.ensureTransientMark();
            this.cmd("forward_sexp");
            this.setMark(this.point());
            this.transientMarker.swap(this.caretMarker);
        });
        this.ensureTransientMark();
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
        let tok = this.tokenizer.finishParsing();
        if (tok) {
            let rc = this._rowcol;
            let lc = { line: rc.row, col: rc.col };
            let p = getPP(tok).filter(p => p.closed).sort(compareRowCol).find(p => compareRowCol(p, lc) >= 0);
            if (p != null) {
                this.cmd("goto_char", this._rowColToPosition(p.line, endOf(p)));
            } else {
                ERROR(this);
            }
        }
    }),

    backward_up_list: Ymacs_Interactive(function(){
        let tok = this.tokenizer.finishParsing();
        if (tok) {
            let rc = this._rowcol;
            let lc = { line: rc.row, col: rc.col };
            let p = getPP(tok).filter(p => p.closed)
                .sort(compareRowCol)
                .findLast(p => compareRowCol(p, lc) < 0 && compareRowCol(p.closed, lc) >= 0);
            if (p != null) {
                this.cmd("goto_char", this._rowColToPosition(p.line, startOf(p)));
            } else {
                ERROR(this);
            }
        }
    }),

    up_list: Ymacs_Interactive(function(){
        this.cmd("backward_up_list");
        this.cmd("forward_sexp");
    }),

    beginning_of_defun: Ymacs_Interactive(function(){
        let tok = this.tokenizer.finishParsing();
        if (tok) {
            let rc = this._rowcol;
            let lc = { line: rc.row, col: rc.col };
            let p = getPP(tok).filter(p => p.closed)
                .sort(compareRowCol)
                .find(p => compareRowCol(p, lc) < 0 && compareRowCol(p.closed, lc) >= 0);
            if (p != null) {
                this.cmd("goto_char", this._rowColToPosition(p.line, startOf(p)));
                this.cmd("back_to_indentation");
            } else {
                this.cmd("backward_sexp");
            }
        }
    }),

    end_of_defun: Ymacs_Interactive(function(){
        let tok = this.tokenizer.finishParsing();
        if (tok) {
            let rc = this._rowcol;
            let lc = { line: rc.row, col: rc.col };
            let p = getPP(tok).filter(p => p.closed)
                .sort(compareRowCol)
                .find(p => compareRowCol(p, lc) < 0 && compareRowCol(p.closed, lc) >= 0);
            if (p != null) {
                p = p.closed;
                this.cmd("goto_char", this._rowColToPosition(p.line, endOf(p)));
            } else {
                this.cmd("forward_sexp");
            }
        }
    }),

    paredit_raise_sexp: Ymacs_Interactive(function(){
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
        var r = this.transientMarker
            ? this.getRegion()
            : this.cmd("save_excursion", function(){
                var begin = this.point();
                this.cmd("forward_sexp");
                return { begin: begin, end: this.point() };
            }),
            txt = this._bufferSubstring(r.begin, r.end),
            before = this.point() < r.end;
        if (backslash) {
            txt = txt.replace(backslash, s => "\\" + s);
        }
        var m = this.createMarker(r.end);
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
            && this.looking_back(new RegExp("\\" + R_PARENS[this.matchData[1]] + "[ \\t]*"));
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

    function inside(a, b, c) {
        return b >= a && b < c;
    }

    let events = {
        beforeInteractiveCommand: function() {
            clearOvl();
        },
        afterInteractiveCommand: delayed(() => {
            let p = this.tokenizer.getLastParser(), rc = this._rowcol;
            let hl = [];
            if (p) for (let par_a of getPP(p)) {
                let par_b = par_a.closed;
                let ac1 = par_a.c1 ?? par_a.col, ac2 = par_a.c2 ?? (par_a.col + par_a.type.length);
                let bc1 = par_b.c1 ?? par_b.col, bc2 = par_b.c2 ?? (par_b.col + 1);
                if (caretInOP(rc, par_a) || caretInCP(rc, par_b)) {
                    hl.push({
                        line1: par_a.line, col1: ac1,
                        line2: par_a.line, col2: ac2
                    }, {
                        line1: par_b.line, col1: bc1,
                        line2: par_b.line, col2: bc2
                    });
                    break;
                }
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
