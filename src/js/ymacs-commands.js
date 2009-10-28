// @require ymacs-buffer.js

Ymacs_Buffer.newCommands({

        forward_char: function(x) {
                if (x == null) x = 1;
                return this._repositionCaret(this.point() + x);
        },

        backward_char: function(x) {
                if (x == null) x = 1;
                return this.cmd("forward_char", -x);
        },

        forward_line: function(x) {
                if (x == null) x = 1;
                var rc = this._rowcol;
                return this._repositionCaret(this._rowColToPosition(rc.row + x, rc.col));
        },

        backward_line: function(x) {
                if (x == null) x = 1;
                return this.cmd("forward_line", -x);
        },

        forward_whitespace: function() {
                if (this.cmd("search_forward_regexp", /[^\s]/g))
                        this.cmd("backward_char");
        },

        backward_whitespace: function() {
                if (this.cmd("search_backward_regexp", /[^\s]/g))
                        this.cmd("forward_char");
        },

        beginning_of_line: function() {
                var rc = this._rowcol;
                return this._repositionCaret(this._rowColToPosition(rc.row, 0));
        },

        back_to_indentation: function() {
                var rc = this._rowcol, line = this.code[rc.row], m = /\S/.exec(line);
                if (m)
                        return this._repositionCaret(this._rowColToPosition(rc.row, m.index));
        },

        beginning_of_indentation_or_line: function() {
                return this.cmd("back_to_indentation") || this.cmd("beginning_of_line");
        },

        end_of_line: function() {
                var rc = this._rowcol;
                return this._repositionCaret(this._rowColToPosition(rc.row, this.code[rc.row].length));
        },

        beginning_of_buffer: function() {
                return this._repositionCaret(0);
        },

        end_of_buffer: function() {
                return this._repositionCaret(this.getCodeSize());
        },

        backward_delete_char: function() {
                var pos = this.point();
                if (pos > 0) {
                        var rc = this._rowcol, line = this.code[rc.row], ch;
                        if (rc.col > 0) {
                                ch = line.charAt(rc.col - 1);
                                line = line.substr(0, rc.col - 1) + line.substr(rc.col);
                                this._replaceLine(rc.row, line);
                        } else {
                                ch = "\n";
                                // merge with the previous line
                                line = this.code[rc.row - 1] + line;
                                this._replaceLine(rc.row - 1, line);
                                this._deleteLine(rc.row);
                        }
                        // *** UNDO RECORDING
                        if (!this.__preventUndo)
                                this._recordChange(2, pos - 1, 1, ch);
                        this._updateMarkers(pos, -1);
                }
        },

        delete_char: function() {
                if (this.cmd("forward_char"))
                        this.cmd("backward_delete_char");
        },

        delete_whitespace: function() {
                var ret = false;
                while (/^\s$/.test(this.charAt())) {
                        ret = true;
                        this.cmd("delete_char");
                }
                return ret;
        },

        backward_delete_whitespace: function() {
                var ret = false;
                while (/^\s$/.test(this.charAt(-1))) {
                        ret = true;
                        this.cmd("backward_delete_char");
                }
                return ret;
        },

        overwrite_mode: function() {
                this.resetOverwriteMode();
        },

        self_insert_command: function(ev) {
                if (!ev)
                        ev = this.interactiveEvent;
                var ch = String.fromCharCode(ev.charCode),
                    rc = this._rowcol;
                if (ev.charCode && ch && !ev.altKey && !ev.ctrlKey) {
                        var line = this.code[rc.row];
                        // *** UNDO RECORDING
                        if (!this.__preventUndo) {
                                if (this.overwriteMode && rc.col < line.length)
                                        this._recordChange(2, this.point(), 1, line.charAt(rc.col));
                                this._recordChange(1, this.point(), 1);
                        }
                        line = line.substr(0, rc.col) + ch + line.substr(this.overwriteMode ? rc.col + 1 : rc.col);
                        this._replaceLine(rc.row, line);
                        this.caretMarker.updateMarkers(+1);
                        ev.domStop = true;
                        return true;
                }
                return false;
        },

        newline: function() {
                var rc = this._rowcol,
                    line = this.code[rc.row],
                    rest = line.substr(rc.col);
                line = line.substr(0, rc.col);
                this._replaceLine(rc.row, line);
                this._insertLine(rc.row + 1, rest);
                // *** UNDO RECORDING
                if (!this.__preventUndo)
                        this._recordChange(1, this.point(), 1);
                this.caretMarker.updateMarkers(+1);
        },

        indent_line: function() {
                this.cmd("insert", "    ");
        },

        insert_text: function(txt) {
                return this._insertText(txt);
        },

        make_marker: function(pos) {
                return this.createMarker(pos);
        },

        looking_at: function(rx) {
                rx.global = true;
                var pos = rx.lastIndex = this.point();
                var ret = this.matchData = rx.exec(this.getCode());
                // console.log(ret, ret && ret.index);
                return ret && ret.index == pos;
        },

        looking_back: function(rx, bound) {
                rx.global = true;
                if (bound < 0)
                        bound += this.point();
                var pos = this.point();
                var index = this.lastIndexOfRegexp(this.getCode(), rx, pos, bound)[0];
                return pos == index;
        },

        search_forward: function(str) {
                var code = this.getCode(), point = this.point();
                if (this.getq("case_fold_search")) {
                        code = code.toLowerCase();
                        str = str.toLowerCase();
                }
                var pos = code.indexOf(str, point);
                if (pos >= 0) {
                        this._repositionCaret(pos + str.length);
                        return true;
                }
        },

        search_backward: function(str) {
                var code = this.getCode(), point = this.point();
                if (this.getq("case_fold_search")) {
                        code = code.toLowerCase();
                        str = str.toLowerCase();
                }
                var pos = code.lastIndexOf(str, point);
                if (pos == point)
                        pos = code.lastIndexOf(str, point - 1);
                if (pos >= 0 && pos != point) {
                        this._repositionCaret(pos);
                        return true;
                }
        },

        search_forward_regexp: function(rx) {
                rx.global = true;
                var code = this.getCode();
                var pos = rx.lastIndex = this.point();
                var ret = this.matchData = rx.exec(code);
                if (ret && rx.lastIndex != pos) {
                        this._repositionCaret(rx.lastIndex);
                        return true;
                }
        },

        search_backward_regexp: function(rx) {
                rx.global = true;
                var pos = this.point();
                var index = this.lastIndexOfRegexp(this.getCode(), rx, pos)[1];
                if (index != null && index != pos) {
                        this._repositionCaret(index);
                        return true;
                }
        },

        forward_word: function() {
                var word = this.syntax.word_ng, end = false;
                while (!end && !word.test(this.charAt()))
                        if (!this.cmd("forward_char"))
                                end = true;
                while (!end && word.test(this.charAt()))
                        if (!this.cmd("forward_char"))
                                end = true;
        },

        backward_word: function() {
                var word = this.syntax.word_ng, end = false;
                while (!end && !word.test(this.charAt(-1)))
                        if (!this.cmd("backward_char"))
                                end = true;
                while (!end && word.test(this.charAt(-1)))
                        if (!this.cmd("backward_char"))
                                end = true;
        },

        forward_paragraph: function() {
                this.cmd("forward_whitespace");
                if (this.cmd("search_forward_regexp", this.syntax.paragraph_sep))
                        this.cmd("goto_char", this.cmd("match_beginning") + 1);
                else
                        this.cmd("end_of_buffer");
        },

        backward_paragraph: function() {
                this.cmd("backward_whitespace");
                if (this.cmd("search_backward_regexp", this.syntax.paragraph_sep))
                        this.cmd("goto_char", this.cmd("match_end") - 1);
                else
                        this.cmd("beginning_of_buffer");
        },

        transpose_words: function() {
                // if we're in the middle of a word, some
                // weird things happen; better skip it, just
                // like Emacs does.
                this.cmd("backward_char");
                if (this.syntax.word_ng.test(this.charAt()))
                        this.cmd("forward_word");

                // // save next word position
                // var wp1 = this.cmd("save_excursion", function(){
                //         this.cmd("forward_word");
                //         var p1 = this.point();
                //         this.cmd("backward_word");
                //         return [ p1, this.point() ];
                // });

                // // save previous word position
                // var wp2 = this.cmd("save_excursion", function(){
                //         this.cmd("backward_word");
                //         var p1 = this.point();
                //         this.cmd("forward_word");
                //         return [ p1, this.point() ];
                // });

                // this.cmd("goto_char", this._swapAreas(wp1.concat(wp2)));

                var a = [];
                this.cmd("forward_word"); a.push(this.point());
                this.cmd("backward_word"); a.push(this.point());
                this.cmd("backward_word"); a.push(this.point());
                this.cmd("forward_word"); a.push(this.point());
                this.cmd("goto_char", this._swapAreas(a));
        },

        transpose_lines: function() {
                var a = [];
                this.cmd("backward_line");
                this.cmd("beginning_of_line"); a.push(this.point());
                this.cmd("end_of_line"); a.push(this.point());
                this.cmd("forward_char"); a.push(this.point());
                this.cmd("end_of_line"); a.push(this.point());
                this.cmd("goto_char", this._swapAreas(a) + 1);
        },

        transpose_chars: function() {
                var pos = this.point();
                if (this.cmd("backward_char"))
                        this.cmd("goto_char", this._swapAreas([ pos - 1, pos, pos, pos + 1 ]));
        },

        kill_word: function() {
                var pos = this.point();
                this.cmd("forward_word");
                var pos2 = this.point();
                this._killingAction(pos, pos2, false);
        },

        backward_kill_word: function() {
                var pos = this.point();
                this.cmd("backward_word");
                var pos2 = this.point();
                this._killingAction(pos, pos2, true);
        },

        goto_char: function(pos) {
                return this._repositionCaret(pos);
        },

        insert: function(text) {
                return this._insertText(text);
        },

        kill_line: function(x, wholeLine) {
                if (typeof x == "number" && x != 0)
                        this.cmdRepeat(x, "kill_line", 0, true);
                var pos = this.point(),
                rc = this._rowcol,
                line = this.code[rc.row],
                end = pos + line.length - rc.col;
                if (rc.row < this.code.length - 1 && (wholeLine || this.cmd("looking_at", /\s*$/mg)))
                        end++;
                this._killingAction(pos, end);
        },

        save_excursion: function() {
                return this._saveExcursion.apply(this, arguments);
        },

        point: function() {
                return this.caretMarker.getPosition();
        },

        kill_region: function() {
                this._killingAction(this.caretMarker, this.markMarker);
        },

        copy_region_as_kill: function() {
                this._killingAction(this.caretMarker, this.markMarker, false, true);
        },

        yank: function() {
                var point = this.point();
                this._insertText(this.ymacs.killRingText());
                this.cmd("set_mark_command", point);
        },

        yank_pop: function() {
                if (/^yank/.test(this.previousCommand)) {
                        this.ymacs.rotateKillRing(false);
                        this._deleteText(this.caretMarker, this.markMarker);
                        this.cmd("yank");
                } else {
                        this.signalError("Previous command was not a yank");
                }
        },

        yank_shift: function() {
                if (/^yank/.test(this.previousCommand)) {
                        this.ymacs.rotateKillRing(true);
                        this._deleteText(this.caretMarker, this.markMarker);
                        this.cmd("yank");
                } else {
                        this.signalError("Previous command was not a yank");
                }
        },

        mark: function() {
                return this.markMarker.getPosition();
        },

        set_mark_command: function(x) {
                if (x == null) x = this.point();
                this.markMarker.setPosition(x);
        },

        exchange_point_and_mark: function() {
                this.caretMarker.swap(this.markMarker);
                this.cmd("recenter_top_bottom");
        },

        recenter_top_bottom: function() {
                this._centerOnCaret();
        },

        _apply_operation_on_word: function (op) {
                var pos = this.point();
                if (this.syntax.word_ng.test(this.charAt())) {
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
                                this.cmd(this.currentCommand);
                }
        },

        capitalize_word: function() {
                this.cmd("_apply_operation_on_word", function() {
                        return this.charAt(0).toUpperCase() + this.substr(1).toLowerCase();
                });
        },

        downcase_word: function() {
                this.cmd("_apply_operation_on_word", String.prototype.toLowerCase);
        },

        upcase_word: function() {
                this.cmd("_apply_operation_on_word", String.prototype.toUpperCase);
        },

        fill_paragraph: function() {
                this.cmd("save_excursion", function(){
                        if (!this.cmd("looking_at", this.syntax.paragraph_sep))
                                this.cmd("forward_paragraph");
                        var eop = this.createMarker(this.point() - 1);
                        this.cmd("backward_paragraph");
                        this.cmd("forward_char");

                        // remove newlines first
                        // console.time("newlines");
                        while (true) {
                                this.cmd("end_of_line");
                                this.cmd("backward_delete_whitespace");
                                if (this.point() >= eop.getPosition())
                                        break;
                                this._replaceText(this.point(), this.point() + 1, " ");
                        }
                        // console.timeEnd("newlines");

                        this.cmd("beginning_of_line");

                        // main operation
                        // console.time("filling");
                        while (this.point() < eop.getPosition()) {
                                this.cmd("forward_word");
                                if (this._rowcol.col > this.getq("fill_column")) {
                                        this.cmd("backward_word");
                                        this.cmd("backward_delete_whitespace");
                                        this.cmd("newline");
                                }
                        }
                        // console.timeEnd("filling");

                        eop.destroy();
                });
        },

        scroll_down: function() {
                this.whenActiveFrame(function(frame){
                        var hl = frame.heightInLines();
                        this.cmd("forward_line", Math.round(hl / 1.33));
                        this.cmd("recenter_top_bottom");
                });
        },

        scroll_up: function() {
                this.whenActiveFrame(function(frame){
                        var hl = frame.heightInLines();
                        this.cmd("backward_line", Math.round(hl / 1.33));
                        this.cmd("recenter_top_bottom");
                });
        },

        nuke_trailing_whitespace: function() {
                this.cmd("save_excursion", function(){
                        this.cmd("goto_char", 0);
                        while (this._rowcol.row < this.code.length) {
                                var line = this.code[this._rowcol.row],
                                m = /\s+$/.exec(line);
                                if (m)
                                        this._deleteText(this.point() + m.index, this.point() + line.length);
                                if (!this.cmd("forward_line"))
                                        break;
                        }
                }, true);
        },

        match_string: function(n) {
                return this.matchData[n];
        },

        match_beginning: function() {
                return this.matchData.index;
        },

        match_end: function() {
                return this.matchData.index + this.matchData[0].length;
        },

        undo: function() {
                var q = this.__undoQueue;
                this.__undoQueue = this.__redoQueue;
                this._placeUndoBoundary();
                if (!this._playbackUndo(q)) {
                        this.signalError("No further undo information");
                }
                this.__undoQueue = q;
        },

        center_line: function() {
                this.cmd("save_excursion", function(){
                        this.cmd("end_of_line");
                        this.cmd("backward_delete_whitespace");
                        this.cmd("beginning_of_line");
                        this.cmd("delete_whitespace");
                        var line = this.code[this._rowcol.row];
                        var indent = Math.floor((this.getq("fill_column") - line.length) / 2);
                        this.cmd("insert", " ".x(indent));
                });
        },

        delete_region_or_line: function() {
                // right now this just deletes the line, since there's
                // no notion of transient region
                this.cmd("beginning_of_line");
                var pos = this.point();
                if (this.cmd("forward_line") || this.cmd("end_of_line")) {
                        this._deleteText(pos, this.point());
                        return true;
                }
        },

        bind_variables: function() {
                return this.withVariables.apply(this, arguments);
        },

        buffer_substring: function(start, end) {
                return this._bufferSubstring(start, end);
        }

});

/* -----[ commands to help using the system clipboard ]----- */

Ymacs_Buffer.newCommands((function(){

        // <XXX>
        // this should be moved some level up if it proves to be more
        // generably useful
        function modalTextarea(title, text, cont) {
                var dlg = this.createDialog({ title   : title,
                                              quitBtn : "destroy",
                                              modal   : true });
                var entry = new DlEntry({ parent: dlg, type: "textarea", fillParent: true, value: text });
                dlg._focusedWidget = entry;
                dlg.setSize({ x: 350, y: 250 });
                entry.addEventListener("onKeyPress", function(ev){
                        if (ev.keyCode != DlKeyboard.ESCAPE) {
                                cont.call(this, entry);
                                dlg.destroy();
                        }
                }.clearingTimeout(2, this));
                dlg.show(true);
                entry.select();
        };
        // </XXX>

        return {

                yank_from_operating_system: function() {
                        modalTextarea.call(this, "Paste below (press CTRL-V)", null, function(entry){
                                this.cmd("insert", entry.getValue());
                                this.cmd.delayed(20, this, "recenter_top_bottom");
                        });
                },

                copy_for_operating_system: function() {
                        var text = this._bufferSubstring(this.caretMarker, this.markMarker);
                        modalTextarea.call(this, "Press CTRL-C", text, function(entry){
                                this.cmd("copy_region_as_kill");
                        });
                },

                kill_for_operating_system: function() {
                        var text = this._bufferSubstring(this.caretMarker, this.markMarker);
                        modalTextarea.call(this, "Press CTRL-C or CTRL-X", text, function(entry){
                                this.cmd("kill_region");
                        });
                }

        };

})());
