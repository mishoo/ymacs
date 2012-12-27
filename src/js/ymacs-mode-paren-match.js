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
// @require ymacs-tokenizer.js

DEFINE_SINGLETON("Ymacs_Keymap_ParenMatch", Ymacs_Keymap, function(D, P) {

    D.KEYS = {
        "C-c \\"                       : "goto_matching_paren",
        "C-M-q"                        : "indent_sexp",
        "C-M-f && C-M-n"               : "forward_sexp",
        "C-M-b && C-M-p"               : "backward_sexp",
        "C-M-u && M-a && C-M-ARROW_UP" : "backward_up_list",
        "M-e"                          : "up_list",
        "C-M-ARROW_DOWN"               : "down_list",
        "M-C-k"                        : "kill_sexp",
        "M-C-SPACE"                    : "mark_sexp",
        "M-C-t"                        : "transpose_sexps",
        "M-("                          : [ "paredit_wrap_round", "(" ],
        "M-["                          : [ "paredit_wrap_round", "[" ],
        "M-{"                          : [ "paredit_wrap_round", "{" ],
        'M-"'                          : [ "paredit_wrap_round", '"' ],
        "M-'"                          : [ "paredit_wrap_round", "'" ],
        "M-r"                          : "paredit_raise_sexp",
        "M-s"                          : "paredit_splice_sexp",
        "BACKSPACE"                    : "paredit_backward_delete_char",
        "DELETE && C-d"                : "paredit_delete_char"
    };

    /* -----[ new commands ]----- */

    function compareRowCol(p1, p2) {
        return (p1.line < p2.line)
            ? -1
            : p1.line > p2.line
            ? 1
            : p1.col - p2.col;
    };

    var PARENS = {
        "(" : ")",
        "[" : "]",
        "{" : "}",
        '"' : { close: '"', backslash: /[\x22\\]/g },
        "'" : { close: "'", backslash: /[\x27\\]/g }
    };

    var R_PARENS = {
        ")" : "(",
        "]" : "[",
        "}" : "{",
        '"' : '"'
    };

    function ERROR(o) {
        throw new Ymacs_Exception("Balanced expression not found");
    };

    function getPP(p) {
        var pp = p.context.passedParens;
        return pp instanceof Function ? pp() : pp;
    };

    Ymacs_Buffer.newCommands({

        matching_paren: function() {
            var p = this.tokenizer.getLastParser(), rc = this._rowcol;
            if (p) {
                var parens = getPP(p);
                return parens.foreach(function(p){
                    var match = p.closed;
                    if (p.line == rc.row && p.col == rc.col) {
                        $RETURN( this._rowColToPosition(match.line, match.col + 1) );
                    } else if (match.line == rc.row && match.col == rc.col - 1) {
                        $RETURN( this._rowColToPosition(p.line, p.col) );
                    }
                }, this);
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
            var rc = this._rowcol, p = this.tokenizer.finishParsing();
            if (p) {
                // find next paren
                var parens = getPP(p).mergeSort(compareRowCol);
                var next = parens.foreach(function(p){
                    if (p.line > rc.row || (p.line == rc.row && p.col >= rc.col)) {
                        $RETURN(p);
                    }
                });
                if (!next || !next.closed) {
                    ERROR(this);
                    return;
                }
                var start = this._rowColToPosition(next.line, next.col);
                if ((this._rowcol.row == next.line && this._rowcol.col == next.col)
                    || !/\S/.test(this._bufferSubstring(null, start)))
                    this.cmd("goto_char", this._rowColToPosition(next.closed.line, next.closed.col) + 1);
                else
                    this.cmd("goto_char", start);
                return true;
            }
        }),

        backward_sexp: Ymacs_Interactive(function() {
            var rc = this._rowcol, p = this.tokenizer.finishParsing();
            if (p) {
                // find next paren
                var parens = getPP(p).grep("closed").map("closed").mergeSort(compareRowCol);
                var prev = parens.r_foreach(function(p){
                    if (p.line < rc.row || (p.line == rc.row && p.col < rc.col))
                        $RETURN(p);
                });
                if (!prev) {
                    ERROR(this);
                    return;
                }
                this.cmd("goto_char", this._rowColToPosition(prev.opened.line, prev.opened.col));
                return true;
            }
        }),

        mark_sexp: Ymacs_Interactive("^r", function(begin, end){
            this.cmd("save_excursion", function(){
                if (this.transientMarker)
                    this.cmd("goto_char", end);
                this.ensureTransientMark();
                this.cmd("forward_sexp");
                this.cmd("set_mark_command", this.point());
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

        paredit_wrap_round: Ymacs_Interactive("^", function(paren, nosexp){
            if (!paren)
                paren = "(";
            var closing = PARENS[paren],
            r = this.transientMarker
                ? this.getRegion()
                : this.cmd("save_excursion", function(){
                    var begin = this.point();
                    if (!nosexp)
                        this.cmd("forward_sexp");
                    return { begin: begin, end: this.point() };
                }),
            txt = this._bufferSubstring(r.begin, r.end),
            before = this.point() < r.end;
            if (typeof closing != "string") {
                txt = txt.replace(closing.backslash, function(s){
                    return "\\" + s;
                });
                closing = closing.close;
            }
            var m = this.createMarker(r.end);
            this.cmd("save_excursion", function(){
                this._replaceText(r.begin, r.end, paren + txt + closing);
            }, before);
            this.cmd("forward_char", before ? 1 : -1);
            this.clearTransientMark();
            this.cmd("indent_region", r.begin, m.getPosition());
            m.destroy();
        }),

        down_list: Ymacs_Interactive(function(){
            var rc = this._rowcol, p = this.tokenizer.finishParsing();
            if (p) {
                var lc = { line: rc.row, col: rc.col };
                p = getPP(p).grep("closed").mergeSort(compareRowCol).grep_first(function(p){
                    return compareRowCol(p, lc) >= 0;
                });
                if (p != null) {
                    this.cmd("goto_char", this._rowColToPosition(p.line, p.col) + 1);
                } else {
                    ERROR(this);
                }
            }
        }),

        backward_up_list: Ymacs_Interactive(function(){
            var rc = this._rowcol, p = this.tokenizer.finishParsing();
            if (p) {
                var lc = { line: rc.row, col: rc.col };
                p = getPP(p).grep("closed").mergeSort(compareRowCol).grep_last(function(p){
                    return compareRowCol(p, lc) < 0 && compareRowCol(p.closed, lc) >= 0;
                });
                if (p != null) {
                    this.cmd("goto_char", this._rowColToPosition(p.line, p.col));
                } else {
                    ERROR(this);
                }
            }
        }),

        up_list: Ymacs_Interactive(function(){
            this.cmd("backward_up_list");
            this.cmd("forward_sexp");
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
                if (this.cmd("looking_back", /[\(\[\{\"]/g)) {
                    var close = PARENS[this.matchData[0]];
                    if (close) {
                        if (close.close) close = close.close;
                        var rx = new RegExp("\\s*\\" + close, "mg");
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
                if (this.cmd("looking_at", /[\]\}\)\"]/g)) {
                    var open = R_PARENS[this.matchData[0]];
                    if (open) {
                        var rx = new RegExp("\\" + open + "\\s*", "mg");
                        if (this.cmd("looking_back", rx)) this.cmd("save_excursion", function(){
                            this.cmd("backward_delete_whitespace");
                            this.cmd("backward_delete_char");
                        })
                    }
                }
                this.cmd("delete_char");
            }
        })

    });

    Ymacs_Buffer.newMode("paren_match_mode", function(){

        var keymap = Ymacs_Keymap_ParenMatch();
        this.pushKeymap(keymap);

        var active = false,
        clearOvl = function() {
            if (active)
                this.deleteOverlay("match-paren");
        }.clearingTimeout(500, this);

        var events = {
            beforeInteractiveCommand: function() {
                clearOvl.doItNow();
            },
            afterInteractiveCommand: function() {
                var p = this.tokenizer.getLastParser(), rc = this._rowcol;
                if (p) {
                    getPP(p).foreach(function(p){
                        var match = p.closed;
                        if ((p.line == rc.row && p.col == rc.col) ||
                            (match.line == rc.row && match.col == rc.col - 1)) {
                            active = true;
                            this.setOverlay("match-paren", {
                                line1: p.line, line2: match.line,
                                col1: p.col, col2: match.col + 1
                            });
                            clearOvl();
                        }
                    }, this);
                }
            }.clearingTimeout(100)
        };
        this.addEventListener(events);

        return function() {
            clearOvl.doItNow();
            this.popKeymap(keymap);
            this.removeEventListener(events);
        };

    });

});
