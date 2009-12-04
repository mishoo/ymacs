// This file is part of Ymacs, an extensible source code editor
// (c) Mihai Bazon 2009 <mihai.bazon@gmail.com>
// Distributed under a BSD-style license.
// http://www.ymacs.org/

// @require ymacs-buffer.js

Ymacs_Buffer.newCommands({

        get_region: function() {
                return this.getRegion();
        },

        cperl_lineup: function() {
                this.cmd("save_excursion", function(){
                        var r = this.getRegion(), rcend = this._positionToRowCol(r.end), max = 0, lines = [];
                        this.cmd("goto_char", r.begin);
                        this.cmd("forward_whitespace", true);
                        var ch = this.charAt();
                        if (ch.toLowerCase() != ch.toUpperCase()) {
                                this.signalError("Cannot lineup here");
                                return;
                        }
                        while (this._rowcol.row <= r.end) {
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
                        lines.foreach(function(l){
                                this.cmd("goto_char", this._rowColToPosition(l[0], l[1]));
                                this.cmd("insert", " ".x(max - l[1]));
                        }, this);
                });
        },

        htmlize_region: function() {
                var r = this.cmd("get_region");
                this.tokenizer.finishParsing();
                var row = this._positionToRowCol(r.begin).row,
                    end = this._positionToRowCol(r.end).row,
                    html = String.buffer();
                while (row <= end) {
                        html(this._textProperties.getLineHTML(row, this.code[row], null), "\n");
                        row++;
                }
                html = html.get();
                var tmp = this.ymacs.switchToBuffer("*Htmlize*");
                tmp.setCode(html);
                tmp.cmd("xml_mode", true);
        },

        eval_region: function() {
                var r = this.cmd("get_region"),
                    code = this.cmd("buffer_substring", r.begin, r.end),
                    buffer = this;
                code = "(function(){" + code + "}).call(buffer)";
                try {
                        eval(code);
                } catch(ex) {
                        this.signalError(ex.name + ": " + ex.message);
                }
        }

});
