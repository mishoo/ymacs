// This file is part of Ymacs, an extensible source code editor
// (c) Mihai Bazon 2009 <mihai.bazon@gmail.com>
// Distributed under a BSD-style license.
// http://www.ymacs.org/

// @require ymacs-keymap.js

DEFINE_SINGLETON("Ymacs_Keymap_ISearch", Ymacs_Keymap, function(D, P){

        D.KEYS = {
                "C-g && ESCAPE": [ "isearch_abort", true ],
                "C-w": "isearch_yank_word_or_char",
                "C-s": "isearch_forward",
                "C-r": "isearch_backward",
                "BACKSPACE": function() {
                        if (this.getMinibuffer().point() > this._isearchContext.mbMark.getPosition()) {
                                this.getMinibuffer().cmd("backward_delete_char");
                                this.cmd("goto_char", this._isearchContext.point);
                                updateIsearch.call(this, this._isearchContext.forward);
                        }
                },
                "ENTER": "isearch_abort"
        };

        D.CONSTRUCT = function() {
                this.defaultHandler = [ "isearch_printing_char" ];
        };

        function initIsearch(fw) {
                if (!this._isearchContext) {
                        this.pushKeymap(this._keymap_isearch);
                        this.cmd("set_mark_command");
                        this.setMinibuffer(fw ? "I-Search: " : "I-Search backward: ");
                        this._isearchContext = {
                                forward : fw,
                                point   : this.point(),
                                mbMark  : this.getMinibuffer().createMarker(null, true)
                        };
                        return true;
                }
        };

        function updateIsearch(fw) {
                this._isearchContext.forward = fw;
                this._isearchContext.point = this.point();
                var text = getText(this);
                if (!/\S/.test(text) && this._isearchLastText) {
                        this.getMinibuffer()._placeUndoBoundary();
                        this.getMinibuffer().cmd("insert", this._isearchLastText);
                        text = this._isearchLastText;
                }
                return doSearch.call(this, text);
        };

        function doSearch(text) {
                if (text == null)
                        text = getText(this);
                var found = this.cmd("bind_variables", { case_fold_search: text == text.toLowerCase() },
                                     this.cmd,
                                     this._isearchContext.forward ? "search_forward" : "search_backward",
                                     text);
                if (found)
                        this.cmd("recenter_top_bottom");
                return found;
        };

        function getText(o) {
                return o.cmd("isearch_get_search_text");
        };

        Ymacs_Buffer.newCommands({

                isearch_get_search_text: function() {
                        if (this._isearchContext) {
                                return this.getMinibuffer()._bufferSubstring(this._isearchContext.mbMark);
                        }
                },

                isearch_forward: function() {
                        if (!initIsearch.call(this, true)) {
                                if (!updateIsearch.call(this, true))
                                        this.signalError("No more forward occurrences of the search text");
                        }
                },

                isearch_forward_regexp: function() {
                        this.signalError("Not implemented, but should be easy.  Volunteers?");
                },

                isearch_backward_regexp: function() {
                        this.signalError("Not implemented, but should be easy.  Volunteers?");
                },

                isearch_backward: function() {
                        if (!initIsearch.call(this, false)) {
                                if (!updateIsearch.call(this, false))
                                        this.signalError("No more backward occurrences of the search text");
                        }
                },

                isearch_yank_word_or_char: function() {
                        var pos = this.point();
                        this.cmd("forward_word");
                        var pos2 = this.point();
                        if (pos2 != pos) {
                                var word = this._bufferSubstring(pos, pos2);
                                this.getMinibuffer()._placeUndoBoundary();
                                this.getMinibuffer().cmd("insert", word.toLowerCase());
                        }
                },

                isearch_printing_char: function() {
                        var ev = this.interactiveEvent;
                        if (ev.charCode && !ev.ctrlKey && !ev.altKey) {
                                this.getMinibuffer().cmd("self_insert_command", ev);
                                var text = getText(this);
                                this.cmd("goto_char", this._isearchContext.point);
                                doSearch.call(this, text);
                                return ev.domStop = true;
                        } else if (ev.keyCode != 0 || ev.ctrlKey || ev.altKey) {
                                this.cmd("isearch_abort");
                                return false;
                        }
                },

                isearch_abort: function(cancelled) {
                        if (!cancelled)
                                this._isearchLastText = getText(this);
                        this.setMinibuffer("");
                        this.popKeymap(this._keymap_isearch);
                        this._isearchContext.mbMark.destroy();
                        this._isearchContext = null;
                        if (cancelled)
                                this.cmd("exchange_point_and_mark");
                        return true;
                }

        });

});
