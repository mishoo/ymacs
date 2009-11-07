// @require ymacs-buffer.js

Ymacs_Buffer.newCommands({

        forward_char: function(x) {
                if (x == null) x = 1;
                return this.cmd("goto_char", this.point() + x);
        },

        backward_char: function(x) {
                if (x == null) x = 1;
                return this.cmd("forward_char", -x);
        },

        forward_line: function(x) {
                if (x == null) x = 1;
                var rc = this._rowcol;
                if (!/^(forward|backward)_line$/.test(this.previousCommand)) {
                        this.setq("line_movement_requested_col", rc.col);
                }
                var ret = this.cmd("goto_char",
                                   this._rowColToPosition(rc.row + x,
                                                          Math.max(rc.col,
                                                                   this.getq("line_movement_requested_col")))); // starting to look like Lisp, eh?
                if (!ret)
                        this.setq("line_movement_requested_col", rc.col);
                return ret;
        },

        backward_line: function(x) {
                if (x == null) x = 1;
                return this.cmd("forward_line", -x);
        },

        forward_whitespace: function(noLine) {
                var re = noLine ? /[^\x20\t\xA0]/g : /[^\s]/g;
                if (this.cmd("search_forward_regexp", re)) {
                        this.cmd("backward_char");
                        return true;
                }
        },

        backward_whitespace: function(noLine) {
                var re = noLine ? /[^\x20\t\xA0]/g : /[^\s]/g;
                if (this.cmd("search_backward_regexp", re)) {
                        this.cmd("forward_char");
                        return true;
                }
        },

        beginning_of_line: function() {
                return this.cmd("goto_char", this._rowColToPosition(this._rowcol.row, 0));
        },

        back_to_indentation: function() {
                var rc = this._rowcol, line = this.code[rc.row], m = /\S/.exec(line);
                if (m)
                        return this.cmd("goto_char", this._rowColToPosition(rc.row, m.index));
        },

        beginning_of_indentation_or_line: function() {
                return this.cmd("back_to_indentation") || this.cmd("beginning_of_line");
        },

        end_of_line: function() {
                var rc = this._rowcol;
                return this.cmd("goto_char", this._rowColToPosition(rc.row, this.code[rc.row].length));
        },

        beginning_of_buffer: function() {
                return this.cmd("goto_char", 0);
        },

        end_of_buffer: function() {
                return this.cmd("goto_char", this.getCodeSize());
        },

        eob_p: function() {
                return this.point() == this.getCodeSize();
        },

        bob_p: function() {
                return this.point() == 0;
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

        delete_whitespace: function(noLine) {
                var p = this.point();
                if (this.cmd("forward_whitespace", noLine)) {
                        this._deleteText(p, this.point());
                        return true;
                }
        },

        backward_delete_whitespace: function(noLine) {
                var p = this.point();
                if (this.cmd("backward_whitespace", noLine)) {
                        this._deleteText(this.point(), p);
                        return true;
                }
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
                this.cmd("insert", "\n");
        },

        newline_and_indent: function() {
                // this.cmd("backward_delete_whitespace", true);
                this.cmd("newline");
                this.cmd("indent_line");
        },

        indent_line: function() {
                if (this.tokenizer) {
                        var indent = this.tokenizer.getIndentation(this._rowcol.row);
                        if (indent != null) {
                                var pos = this.cmd("save_excursion", function(){
                                        this.cmd("back_to_indentation");
                                        if (this._rowcol.col != indent) {
                                                this.cmd("beginning_of_line");
                                                while (/^[ \t\xa0]$/.test(this.charAt()))
                                                        this.cmd("delete_char");
                                                this.cmd("insert", " ".x(indent));
                                        }
                                        return this.point();
                                });
                                // when point is before the indentation, go there.
                                if (this.point() < pos)
                                        this.cmd("goto_char", pos);
                                return;
                        }
                }
                this.cmd("insert", "    ");
        },

        make_marker: function(pos) {
                return this.createMarker(pos);
        },

        looking_at: function(rx) {
                var pos = rx.lastIndex = this.point();
                var ret = this.matchData = rx.exec(this.getCode());
                if (ret)
                        ret.after = rx.lastIndex;
                // console.log(ret, ret && ret.index);
                return ret && ret.index == pos;
        },

        looking_back: function(rx) {
                var m = this.lastIndexOfRegexp(this.getCode(), rx, this.point());
                return m && m.after == this.point();

                /* Crap, the following is slower and buggy.  We really
                 * can't do any better without better RegExp support
                 * from the browser.
                 */
                // rx = Ymacs_Regexp.looking_back(rx);
                // var m = rx.exec(this.getCode().substring(0, this.point()));
                // if (m) {
                //         this.matchData = m;
                //         m.after = this.point();
                //         return true;
                // }
        },

        search_forward: function(str) {
                var code = this.getCode(), point = this.point();
                if (this.getq("case_fold_search")) {
                        code = code.toLowerCase();
                        str = str.toLowerCase();
                }
                var pos = code.indexOf(str, point);
                if (pos >= 0) {
                        this.cmd("goto_char", pos + str.length);
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
                        this.cmd("goto_char", pos);
                        return true;
                }
        },

        search_forward_regexp: function(rx) {
                var code = this.getCode();
                var pos = rx.lastIndex = this.point();
                var ret = this.matchData = rx.exec(code);
                if (ret && rx.lastIndex != pos) {
                        ret.after = rx.lastIndex;
                        this.cmd("goto_char", rx.lastIndex);
                        return true;
                }
        },

        search_backward_regexp: function(rx) {
                var m = this.lastIndexOfRegexp(this.getCode(), rx, this.point());
                if (m && m.index != this.point()) {
                        this.cmd("goto_char", m.index);
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

        goto_char: function(pos) {
                return this._repositionCaret(pos);
        },

        insert: function() {
                return this._insertText(Array.$(arguments).join(""));
        },

        buffer_substring: function(start, end) {
                return this._bufferSubstring(start, end);
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

        /* -----[ paragraphs ]----- */

        fill_paragraph: function(noPrefix) {
                this.cmd("save_excursion", function(){
                        if (!this.cmd("looking_at", this.syntax.paragraph_sep))
                                this.cmd("forward_paragraph");
                        var eop = this.createMarker(this.point() - 1);
                        this.cmd("backward_paragraph");
                        if (this.point() > 0)
                                this.cmd("forward_char");

                        // identify the prefix to use for each line
                        var prefix = "", del = false;
                        if (this.cmd("looking_at", /\s*([-]|[0-9]+\.|\(?[a-z][\).])?\s+/ig)) {
                                prefix = " ".x(this.matchData[0].length);
                                del = /\s*[#>;\s]*\s*/g;
                        }
                        else if (this.cmd("looking_at", /\s*[#>;*\s]+\s*/g)) {
                                prefix = this.matchData[0];
                                del = /\s*[#>;\s]*\s*/g;
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
                        while (this.point() < eop.getPosition()) {
                                var p = this.point();
                                if (!this.cmd("search_forward_regexp", /\s/g))
                                        break;
                                if (this.point() > eop.getPosition()) {
                                        this.cmd("goto_char", eop);
                                }
                                if (this._rowcol.col > this.getq("fill_column")) {
                                        this.cmd("goto_char", p);
                                        this.cmd("backward_delete_whitespace");
                                        this.cmd("newline");
                                        this.cmd("insert", prefix);
                                }
                        }

                        eop.destroy();

                        this.cmd("recenter_top_bottom");
                });
        },

        fill_paragraph_no_prefix: function() {
                return this.cmd("fill_paragraph", true);
        },

        // this looks at the style of the current paragraph and starts
        // a similar one, i.e. using same indentation level and prefix
        // (list-like prefixes are incremented)
        start_next_paragraph: function() {
                this.cmd("backward_paragraph");
                if (this.point() > 0)
                        this.cmd("forward_char");

                // identify the prefix to use for each line
                var prefix = "";
                if (this.cmd("looking_at", /(\s*)([0-9]+)(\.\s+)/g)) {
                        prefix = this.matchData[1] +
                                (parseInt(this.matchData[2], 10) + 1) +
                                this.matchData[3];
                }
                else if (this.cmd("looking_at", /(\s*\(?)([a-z])([\.\)]\s+)/ig)) {
                        prefix = this.matchData[1] +
                                String.fromCharCode(this.matchData[2].charCodeAt(0) + 1) +
                                this.matchData[3];
                }
                else if (this.cmd("looking_at", /\s*[#>;*\s-]+\s*/g)) {
                        prefix = this.matchData[0];
                }

                this.cmd("forward_paragraph");
                if (this.cmd("eob_p"))
                        this.cmd("newline");

                this.cmd("insert", "\n", prefix);

                if (!this.cmd("looking_at", /\n\n/g)) {
                        this.cmd("newline");
                        this.cmd("backward_char");
                };
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
                                        this._deleteText(this.point() + m.index, this.point() + m.index + m[0].length);
                                if (!this.cmd("forward_line"))
                                        break;
                        }
                });
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

        /* -----[ dabbrev ]----- */

        dabbrev_expand: function() {
                if (this.previousCommand != "dabbrev_expand")
                        this.setq("dabbrev_context", null);
                var ctx = this.getq("dabbrev_context");
                if (!ctx) {
                        ctx = this.setq("dabbrev_context", {});
                        var p1 = this.cmd("save_excursion", function(){
                                this.cmd("backward_word");
                                return this.point();
                        });
                        if (p1 == this.point())
                                return this.signalError("Nothing to expand");
                        ctx.search = this.cmd("buffer_substring", p1, this.point());
                        ctx.point = p1;
                        ctx.length = this.point() - p1;
                        ctx.lastSearch = p1;
                        ctx.encountered = {};
                        ctx.forward = false;
                        ctx.buffer = this;
                }
                var expansion;

                // in the following excursion, *this* is ctx.buffer,
                // not necessarily the currently active buffer.  It's
                // purpose is to determine the next expansion and
                // setup the context so that the next invocation would
                // continue.
                ctx.buffer.cmd("save_excursion", function repeat(){
                        var word = this.syntax.word_ng;
                        var p1;
                        var found = false;
                        this.cmd("goto_char", ctx.lastSearch);
                        // console.log("last at: %d", ctx.lastSearch);
                        if (!ctx.forward) {
                                while (this.cmd("search_backward", ctx.search))
                                        if (!word.test(this.charAt(-1))) {
                                                found = true;
                                                break;
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
                                } else
                                        this.signalError("No more completions");
                        }
                        if (p1 != null) {
                                // console.log("%s at %d, next from %d", ctx.search, p1, ctx.lastSearch);
                                this.cmd("forward_word");
                                expansion = this.cmd("buffer_substring", p1, this.point());
                                if (expansion in ctx.encountered)
                                        repeat.call(this);
                        }
                });
                if (expansion) {
                        this._replaceText(ctx.point, ctx.point + ctx.length, expansion);
                        ctx.length = expansion.length;
                        ctx.encountered[expansion] = true;
                }
        },

        /* -----[ sexp-based movement ]----- */

        // these two are awfully slow if they have to search a big
        // text; backward_sexp is triple-awfully slow even if it has
        // to search small text at the end of a big buffer.

        forward_sexp: function() {
                var closed = 0;
                while (!this.cmd("eob_p")) {
                        if (this.cmd("looking_at", /[\[\{\(]/g)) {
                                ++closed;
                                this.cmd("forward_char");
                        }
                        else if (this.cmd("looking_at", /[\]\}\)]/g)) {
                                --closed;
                                this.cmd("forward_char");
                                if (closed == 0)
                                        return true;
                        }
                        else if (this.cmd("looking_at", this.syntax.skip_string)) {
                                this.cmd("goto_char", this.matchData.after);
                        }
                        else if (this.cmd("looking_at", this.syntax.skip_comment)) {
                                this.cmd("goto_char", this.matchData.after);
                        }
                        else this.cmd("forward_char");
                }
        },

        backward_sexp: function() {
                var closed = 0;
                while (!this.cmd("bob_p")) {
                        if (this.cmd("looking_back", /[\]\}\)]/g)) {
                                ++closed;
                                this.cmd("backward_char");
                        }
                        else if (this.cmd("looking_back", /[\[\{\(]/g)) {
                                --closed;
                                this.cmd("backward_char");
                                if (closed == 0)
                                        return true;
                        }
                        else if (this.cmd("looking_back", this.syntax.skip_string)) {
                                this.cmd("goto_char", this.matchData.index);
                        }
                        else if (this.cmd("looking_back", this.syntax.skip_comment)) {
                                this.cmd("goto_char", this.matchData.index);
                        }
                        else this.cmd("backward_char");
                }
        },

        /* -----[ frames and buffers ]----- */

        split_frame_vertically: function() {
                this.whenActiveFrame("vsplit");
        },

        split_frame_horizontally: function() {
                this.whenActiveFrame("hsplit");
        },

        delete_other_frames: function() {
                this.whenActiveFrame("deleteOtherFrames");
        },

        other_frame: function() {
                this.whenYmacs("focusOtherFrame");
        },

        next_buffer: function() {
                this.whenYmacs("switchToNextBuffer");
        },

        previous_buffer: function() {
                this.whenYmacs("switchToPreviousBuffer");
        },

        /* -----[ other ]----- */

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

        // http://mihai.bazon.net/blog/close-last-xml-tag-emacs
        close_last_xml_tag: function() {
                var tag, quote;
                this.cmd("save_excursion", function() {
                        var skip = 1;
                        while (skip != 0 && this.cmd("search_backward_regexp", /<\x2f?([a-zA-Z0-9:_-]+)/g)) {
                                tag = this.cmd("match_string", 1);
                                if (this.cmd("looking_at", /<\x2f/g)) {
                                        ++skip;
                                }
                                else if (!this.cmd("looking_at", /<[^\x2f][^>]*?\x2f>/g)) {
                                        --skip;
                                }
                        }
                        if (skip != 0)
                                tag = null;
                });
                if (tag) {
                        this.cmd("insert", "</", tag, ">");
                } else {
                        this.signalError("Couldn't find a tag to close");
                }
        },

        bind_variables: function() {
                return this.withVariables.apply(this, arguments);
        }

});

/* -----[ commands to help using the system clipboard ]----- */

(function(){

        // <XXX>
        // this should be moved some level up if it proves to be more
        // generally useful
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

        Ymacs_Buffer.newCommands({

                yank_from_operating_system: function() {
                        modalTextarea.call(this, "Paste below (press CTRL-V)", null, function(entry){
                                this.cmd("set_mark_command");
                                var code = entry.getValue().replace(/\t/g, "        ");
                                this.cmd("insert", code);
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

        });

})();
