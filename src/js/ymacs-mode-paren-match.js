// This file is part of Ymacs, an extensible source code editor
// (c) Mihai Bazon 2009 <mihai.bazon@gmail.com>
// Distributed under a BSD-style license.
// http://www.ymacs.org/

// @require ymacs-buffer.js
// @require ymacs-tokenizer.js

DEFINE_CLASS("Ymacs_Keymap_ParenMatch", Ymacs_Keymap, function(D, P) {

        D.KEYS = {
                "C-c \\"         : "goto_matching_paren",
                "C-M-q"          : "indent_sexp",
                "C-M-f && C-M-n" : "forward_sexp",
                "C-M-b && C-M-p" : "backward_sexp",
                "M-C-k"          : "kill_sexp"
        };

        D.CONSTRUCT = function() {
                this.defaultHandler = null; // use next keyboard in buffer.keymap
                this.defineKeys(D.KEYS);
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

                matching_paren: function() {
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
                },

                indent_sexp: function() {
                        var pos = this.cmd("matching_paren");
                        if (pos) {
                                this.cmd("indent_region", this.caretMarker.getPosition(), pos);
                        } else {
                                this.signalError("Balanced expression not found");
                        }
                },

                goto_matching_paren: function() {
                        var pos = this.cmd("matching_paren");
                        if (pos) {
                                this.cmd("goto_char", pos);
                                this.cmd("recenter_top_bottom");
                                return true;
                        }
                },

                forward_sexp: function() {
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
                },

                backward_sexp: function() {
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
                },

                kill_sexp: function() {
                        this._killingAction(
                                this.point(),
                                this.cmd("save_excursion", function() {
                                        this.cmd("forward_sexp");
                                        return this.point();
                                })
                        );
                }

        });

});

Ymacs_Buffer.newMode("paren_match_mode", function(){

        var keymap = new Ymacs_Keymap_ParenMatch({ buffer: this });
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
