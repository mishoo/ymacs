//> This file is part of Ymacs, an Emacs-like editor for the Web
//> http://www.ymacs.org/
//>
//> Copyright (c) 2009, Mihai Bazon, Dynarch.com.  All rights reserved.
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
                "C-c \\"         : "goto_matching_paren",
                "C-M-q"          : "indent_sexp",
                "C-M-f && C-M-n" : "forward_sexp",
                "C-M-b && C-M-p" : "backward_sexp",
                "M-C-k"          : "kill_sexp",
                "M-C-t"          : "transpose_sexps"
        };

        /* -----[ new commands ]----- */

        function compareRowCol(p1, p2) {
                return (p1.line < p2.line)
                        ? -1
                        : p1.line > p2.line
                        ? 1
                        : p1.col - p2.col;
        };

        Ymacs_Buffer.newCommands({

                matching_paren: Ymacs_Interactive(function() {
                        var p = this.tokenizer.getLastParser(), rc = this._rowcol;
                        if (p) {
                                var parens = p.context.passedParens;
                                return parens.foreach(function(p){
                                        var match = p.closed;
                                        if (p.line == rc.row && p.col == rc.col) {
                                                $RETURN( this._rowColToPosition(match.line, match.col + 1) );
                                        } else if (match.line == rc.row && match.col == rc.col - 1) {
                                                $RETURN( this._rowColToPosition(p.line, p.col) );
                                        }
                                }, this);
                        }
                }),

                indent_sexp: Ymacs_Interactive(function() {
                        var pos = this.cmd("matching_paren");
                        if (pos) {
                                this.cmd("indent_region", this.caretMarker.getPosition(), pos);
                        } else {
                                this.signalError("Balanced expression not found");
                        }
                }),

                goto_matching_paren: Ymacs_Interactive(function() {
                        var pos = this.cmd("matching_paren");
                        if (pos) {
                                this.cmd("goto_char", pos);
                                this.cmd("recenter_top_bottom");
                                return true;
                        }
                }),

                forward_sexp: Ymacs_Interactive(function() {
                        var p = this.tokenizer.finishParsing(), rc = this._rowcol;
                        if (p) {
                                // find next paren
                                var parens = p.context.passedParens.mergeSort(compareRowCol);
                                var next = parens.foreach(function(p){
                                        if (p.line > rc.row || (p.line == rc.row && p.col >= rc.col))
                                                $RETURN(p);
                                }, this);
                                if (!next || !next.closed) {
                                        this.signalError("Balanced expression not found");
                                        return;
                                }
                                this.cmd("goto_char", this._rowColToPosition(next.closed.line, next.closed.col) + 1);
                                return true;
                        }
                }),

                backward_sexp: Ymacs_Interactive(function() {
                        var p = this.tokenizer.finishParsing(), rc = this._rowcol;
                        if (p) {
                                // find next paren
                                var parens = p.context.passedParens.grep("closed").map("closed").mergeSort(compareRowCol);
                                var prev = parens.r_foreach(function(p){
                                        if (p.line < rc.row || (p.line == rc.row && p.col < rc.col))
                                                $RETURN(p);
                                }, this);
                                if (!prev) {
                                        this.signalError("Balanced expression not found");
                                        return;
                                }
                                this.cmd("goto_char", this._rowColToPosition(prev.opened.line, prev.opened.col));
                                return true;
                        }
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
                })

        });

});

Ymacs_Buffer.newMode("paren_match_mode", function(){

        var keymap = Ymacs_Keymap_ParenMatch();
        this.pushKeymap(keymap);

        var clearOvl = function() {
                this.deleteOverlay("match-paren");
        }.clearingTimeout(1000, this);

        var events = {
                beforeInteractiveCommand: function() {
                        clearOvl.doItNow();
                },
                afterInteractiveCommand: function() {
                        var p = this.tokenizer.getLastParser(), rc = this._rowcol;
                        if (p) {
                                var parens = p.context.passedParens;
                                parens.foreach(function(p){
                                        var match = p.closed;
                                        if (p.line == rc.row && p.col == rc.col) {
                                                this.setOverlay("match-paren", {
                                                        line1: p.line, line2: match.line,
                                                        col1: p.col, col2: match.col + 1
                                                });
                                                clearOvl();
                                        } else if (match.line == rc.row && match.col == rc.col - 1) {
                                                this.setOverlay("match-paren", {
                                                        line1: p.line, line2: match.line,
                                                        col1: p.col, col2: match.col + 1
                                                });
                                                clearOvl();
                                        }
                                }, this);
                        }
                }.clearingTimeout(250)
        };
        this.addEventListener(events);

        return function() {
                clearOvl.doItNow();
                this.popKeymap(keymap);
                this.removeEventListener(events);
        };

});
