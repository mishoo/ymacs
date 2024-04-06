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

import { Ymacs_Buffer } from "./ymacs-buffer.js";
import { Ymacs_Keymap } from  "./ymacs-keymap.js";
import { Ymacs_Interactive } from "./ymacs-interactive.js";

let Ymacs_Keymap_ISearch = Ymacs_Keymap.define("isearch", {
    "Escape": [ "isearch_abort", true ],
    "C-g": "isearch_reset_or_abort",
    "C-w && C-S-s": "isearch_yank_word_or_char",
    "C-s": "isearch_forward",
    "C-r": "isearch_backward",
    "M-%": "query_replace",
    "Backspace": function() {
        if (this.getMinibuffer().point() > this._isearchContext.mbMark.getPosition()) {
            this.getMinibuffer().cmd("backward_delete_char");
            this.cmd("goto_char", this._isearchContext.point);
            isearchText.call(this);
            updateIsearch.call(this, this._isearchContext.forward);
        }
    },
    "Enter": "isearch_abort"
});
Ymacs_Keymap_ISearch.defaultHandler = [ "isearch_printing_char" ];

function initIsearch(fw) {
    if (!this._isearchContext) {
        this.pushKeymap(Ymacs_Keymap_ISearch);
        this.setMark(this.point());
        this.whenMinibuffer(mb => {
            mb.prompt(fw ? "I-Search: " : "I-Search backward: ");
        });
        this._isearchContext = {
            forward : fw,
            point   : this.point(),
            mbMark  : this.getMinibuffer().createMarker(null, true),
            query   : "",
        };
        return true;
    }
}

function isearchText() {
    return this._isearchContext.query = this.getMinibuffer()._bufferSubstring(this._isearchContext.mbMark);
}

function updateIsearch(fw) {
    this._isearchContext.forward = fw;
    this._isearchContext.point = this.point();
    var query = this._isearchContext.query;
    if (!/\S/.test(query) && this.getq("isearch_last_query")) {
        query = this._isearchContext.query = this.getq("isearch_last_query");
        this.getMinibuffer()._placeUndoBoundary();
        this.getMinibuffer().cmd("insert", query);
    }
    return doSearch.call(this, query);
}

function _lazyHighlight(str) {
    this.deleteOverlay("isearch-lazy");
    if (str) {
        let cursor = this._rowcol;
        let minpos = this._rowColToPosition(cursor.row - 50, 0);
        let maxpos = this._rowColToPosition(cursor.row + 50, Infinity);
        let code = this.getCode();
        if (this.getq("case_fold_search")) {
            code = code.toLowerCase();
            str = str.toLowerCase();
        }
        let pos = minpos, hl = [];
        while (true) {
            pos = code.indexOf(str, pos);
            if (pos < 0 || pos > maxpos) break;
            let p1 = this._positionToRowCol(pos);
            let p2 = this._positionToRowCol(pos += str.length);
            hl.push({
                line1: p1.row, col1: p1.col,
                line2: p2.row, col2: p2.col
            });
        }
        this.setOverlay("isearch-lazy", hl);
    }
}

function lazyHighlight(str) {
    this.cmd("bind_variables", {
        case_fold_search: str != null && str == str.toLowerCase()
    }, _lazyHighlight.bind(this, str));
}

function doSearch(str) {
    return this.cmd("bind_variables", {
        case_fold_search: str == str.toLowerCase()
    }, function() {
        var found = this.cmd(this._isearchContext.forward ? "search_forward" : "search_backward", str);
        if (found) {
            this._isearchContext.lastFoundQuery = str;
            this.cmd("ensure_caret_visible");
            var rc_begin = this._positionToRowCol(this.point() + (this._isearchContext.forward ? -1 : 1) * str.length);
            this.setOverlay("isearch", {
                line1: rc_begin.row, col1: rc_begin.col,
                line2: this._rowcol.row, col2: this._rowcol.col
            });
        }
        lazyHighlight.call(this, str);
        return found;
    });
}

Ymacs_Buffer.newCommands({

    isearch_forward: Ymacs_Interactive(function() {
        if (!initIsearch.call(this, true)) {
            if (!updateIsearch.call(this, true))
                this.signalError("No more forward occurrences of the search text");
        }
    }),

    isearch_forward_regexp: Ymacs_Interactive(function() {
        this.signalError("Not implemented, but should be easy.  Volunteers?");
    }),

    isearch_backward_regexp: Ymacs_Interactive(function() {
        this.signalError("Not implemented, but should be easy.  Volunteers?");
    }),

    isearch_backward: Ymacs_Interactive(function() {
        if (!initIsearch.call(this, false)) {
            if (!updateIsearch.call(this, false)) {
                this.signalError("No more backward occurrences of the search text");
            }
        }
    }),

    isearch_yank_word_or_char: Ymacs_Interactive(function() {
        if (!this._isearchContext) {
            initIsearch.call(this, true);
        }
        var pos = this.point();
        var pos2 = this.cmd("save_excursion", function(){
            this.cmd("forward_word");
            return this.point();
        });
        if (pos2 != pos) {
            var word = this._bufferSubstring(pos, pos2);
            this.getMinibuffer()._placeUndoBoundary();
            this.getMinibuffer().cmd("insert", word.toLowerCase());
            word = isearchText.call(this);
            if (this._isearchContext.forward)
                this.cmd("goto_char", pos2 - word.length);
            doSearch.call(this, word);
        }
    }),

    isearch_printing_char: Ymacs_Interactive(function() {
        var ev = this.interactiveEvent();
        if (ev?.key?.length == 1 && !ev.ctrlKey && !ev.altKey) {
            this.whenMinibuffer(mb => {
                mb.cmd("self_insert_command");
                this.cmd("goto_char", this._isearchContext.point);
                isearchText.call(this);
                if (!doSearch.call(this, this._isearchContext.query)) {
                    let rc = mb._rowcol;
                    mb._textProperties.addLineProps(rc.row, rc.col - 1, rc.col, "css", "isearch-fail");
                }
            });
            return true;
        } else {
            this.cmd("isearch_abort");
            return false;
        }
    }),

    isearch_abort: Ymacs_Interactive(function(cancelled) {
        if (!cancelled)
            this.setGlobal("isearch_last_query", this._isearchContext.query);
        this.setMinibuffer("");
        this.popKeymap(Ymacs_Keymap_ISearch);
        this._isearchContext.mbMark.destroy();
        this._isearchContext = null;
        if (cancelled)
            this.caretMarker.swap(this.markMarker);
        this.deleteOverlay("isearch");
        this.deleteOverlay("isearch-lazy");
        return true;
    }),

    isearch_reset_or_abort: Ymacs_Interactive(function(){
        let ctx = this._isearchContext;
        if (!ctx.lastFoundQuery || ctx.query == ctx.lastFoundQuery) {
            this.cmd("isearch_abort", true);
        } else {
            this.whenMinibuffer(mb => {
                mb._replaceText(ctx.mbMark, mb.getCode().length, ctx.lastFoundQuery);
                doSearch.call(this, ctx.query = ctx.lastFoundQuery);
            });
        }
    }),
});

/* -----[ query-replace ]----- */

let Ymacs_Keymap_Query_Replace = Ymacs_Keymap.define("query_replace", {
    "y"     : "query_replace_yes_this_occurrence",
    "n"     : "query_replace_no_this_occurrence",
    "u"     : "query_replace_undo_previous",
    "!"     : "query_replace_yes_all_occurrences",
    "Enter" : [ "query_replace_abort", true ],
});
Ymacs_Keymap_Query_Replace.defaultHandler = [ "query_replace_abort" ];

function query_replace() {
    this.whenMinibuffer(mb => {
        this.cmd("minibuffer_prompt", "Query replace: ");
        let mbMark = mb.createMarker(null, true);
        let hlOrig = (cmd) => {
            let txt = mb._bufferSubstring(mbMark);
            lazyHighlight.call(this, txt);
        };
        mb.addEventListener("afterInteractiveCommand", hlOrig);
        let onQuit = (continued) => {
            mb.removeEventListener("afterInteractiveCommand", hlOrig);
            mbMark.destroy();
            mb.removeEventListener("abort", onQuit);
            if (!continued) {
                this.deleteOverlay("isearch");
                this.deleteOverlay("isearch-lazy");
            }
        };
        mb.addEventListener("abort", onQuit);
        this.cmd("minibuffer_read_string", null, orig => {
            onQuit(true);
            query_replace_2.call(this, mb, orig);
        });
    });
}

function query_replace_2(mb, orig) {
    lazyHighlight.call(this, orig);
    if (orig) {
        this.cmd("minibuffer_prompt", `Replace “${orig}” with: `);
        let onQuit = () => {
            this.deleteOverlay("isearch");
            this.deleteOverlay("isearch-lazy");
            mb.removeEventListener("abort", onQuit);
        };
        mb.addEventListener("abort", onQuit);
        this.cmd("minibuffer_read_string", null, rplc => {
            query_replace_3.call(this, mb, orig, rplc);
        });
    }
}

function query_replace_3(mb, orig, rplc) {
    mb.setCode(`Replace with “${rplc}”?`);
    this.pushKeymap(Ymacs_Keymap_Query_Replace);
    let cmds, stop, curr, count = 0;
    let gotoNext = (quick) => {
        return this.cmd("bind_variables", {
            case_fold_search: orig == orig.toLowerCase()
        }, function() {
            var found = this.cmd("search_forward", orig);
            if (found) {
                var begin = this.point() - orig.length;
                found = { begin: begin, end: this.point() };
                if (!quick) {
                    this.cmd("ensure_caret_visible");
                    var rc_begin = this._positionToRowCol(begin);
                    this.setOverlay("isearch", {
                        line1: rc_begin.row, col1: rc_begin.col,
                        line2: this._rowcol.row, col2: this._rowcol.col
                    });
                }
            }
            if (!quick) {
                lazyHighlight.call(this, orig);
            }
            return found;
        });
    };
    let queue = [];
    cmds = this.replaceCommands({
        query_replace_yes_this_occurrence: Ymacs_Interactive(() => {
            queue.push(curr);
            this._replaceText(curr.begin, curr.end, rplc);
            count++;
            curr = gotoNext();
            if (!curr) stop();
        }),
        query_replace_no_this_occurrence: Ymacs_Interactive(() => {
            curr = gotoNext();
            if (!curr) stop();
        }),
        query_replace_yes_all_occurrences: Ymacs_Interactive(() => {
            var last;
            while (curr) {
                last = curr;
                this._replaceText(curr.begin, curr.end, rplc);
                count++;
                curr = gotoNext(true);
            }
            if (last) {
                this.cmd("goto_char", last.begin);
                this.cmd("ensure_caret_visible");
            }
            stop();
        }),
        query_replace_undo_previous: Ymacs_Interactive(() => {
            let prev = queue.pop();
            if (prev) {
                count--;
                this.cmd("undo");

                // XXX: this is ugly, but oh well.. We'd like to
                // pretend this edit/undo operation never exist.
                let uptr = this.__undoPointer;
                let uq = this.__undoQueue.slice(0, uptr);
                setTimeout(() => {
                    this.__undoPointer = uptr;
                    this.__undoQueue = uq;
                });

                this.cmd("goto_char", prev.begin);
                curr = gotoNext();
            } else {
                this.signalError("No more undo");
            }
        }),
        query_replace_abort: Ymacs_Interactive(enter => {
            stop();
            return enter;
        }),
    });
    stop = () => {
        mb.removeEventListener("abort", stop);
        this.newCommands(cmds);
        this.deleteOverlay("isearch");
        this.deleteOverlay("isearch-lazy");
        this.popKeymap(Ymacs_Keymap_Query_Replace);
        mb.cmd("minibuffer_keyboard_quit");
        this.signalInfo(`Replaced ${count} occurrences`);
    };
    curr = gotoNext();
    if (!curr) {
        stop();
    } else {
        mb.addEventListener("abort", stop);
    }
}

Ymacs_Buffer.newCommands({
    query_replace: Ymacs_Interactive(function(){
        let ctx = this._isearchContext;
        if (ctx) {
            this.cmd("isearch_abort");
            this.caretMarker.swap(this.markMarker);
            this.whenMinibuffer(mb => {
                query_replace_2.call(this, mb, ctx.query);
            });
        } else {
            query_replace.call(this);
        }
    }),
});
