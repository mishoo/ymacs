/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT

import { Ymacs_Buffer } from "./ymacs-buffer.js";
import { Ymacs_Keymap } from  "./ymacs-keymap.js";
import { Ymacs_Interactive } from "./ymacs-interactive.js";

let Ymacs_Keymap_ISearch = Ymacs_Keymap.define("isearch", {
    "Escape"                : [ "isearch_abort", true ],
    "C-g"                   : "isearch_reset_or_abort",
    "C-w && C-S-s"          : "isearch_yank_word_or_char",
    "C-s"                   : "isearch_forward",
    "C-r"                   : "isearch_backward",
    "M-%"                   : "query_replace",
    "C-M-%"                 : "query_replace_regexp",
    "M-s w && M-w"          : "isearch_toggle_word",
    "M-s c && M-c"          : "isearch_toggle_case_fold",
    "M-s Space && M-Space"  : "isearch_toggle_lax_whitespace",
    "Enter"                 : "isearch_abort",
    "C-l"                   : "recenter_top_bottom",
    "Backspace"             : function() {
        if (this.getMinibuffer().point() > this._isearchContext.mbMark.getPosition()) {
            this.getMinibuffer().cmd("backward_delete_char");
            this.cmd("goto_char", this._isearchContext.livepoint);
            isearchText.call(this);
            updateIsearch.call(this);
        }
    },
});
Ymacs_Keymap_ISearch.defaultHandler = [ "isearch_printing_char" ];

function initIsearch({
    forward = true,
    regexp = false,
    lax = true,
    word = false,
    case_fold = this.getq("case_fold_search"),
    case_replace = this.getq("case_replace"),
    qreplace = false,
    region = null,
} = {}) {
    if (!this._isearchContext) {
        this._isearchContext = {
            forward      : forward,
            regexp       : regexp,
            lax          : lax,
            word         : word,
            case_fold    : case_fold,
            case_replace : case_replace,
            livepoint    : this.point(),
            origpoint    : this.point(),
            current      : { begin: this.point(), end: this.point() },
            mbMark       : this.getMinibuffer().promptMarker,
            query        : "",
            qreplace     : qreplace,
            region       : region,
        };
        if (!qreplace) {
            this.pushKeymap(Ymacs_Keymap_ISearch);
            resetPrompt.call(this);
        }
        return true;
    }
}

function isearchText() {
    return this._isearchContext.query = this.getMinibuffer()._bufferSubstring(this._isearchContext.mbMark);
}

function updateIsearch({ forward } = {}) {
    var query = this._isearchContext.query;
    if (!/\S/.test(query) && this.getq("isearch_last_context")) {
        this._isearchContext = this.getq("isearch_last_context");
        query = this._isearchContext.query;
        this.getMinibuffer()._placeUndoBoundary();
        this.getMinibuffer().cmd("insert", query);
        this._isearchContext.origpoint = this.point();
        this._isearchContext.current = { begin: this.point(), end: this.point() };
    }
    this._isearchContext.livepoint = this.point();
    if (forward != null) {
        this._isearchContext.forward = forward;
    }
    return doSearch.call(this);
}

function caseFold() {
    let ctx = this._isearchContext;
    let query = ctx.query;
    return ctx.case_fold ?? (
        (ctx.regexp ? (query = query.replace(/\\./g, "")) : query) == query.toLowerCase()
    );
}

function lazyHighlight(qrx, prompt) {
    let cursor = this._rowcol;
    let minpos = this._rowColToPosition(cursor.row - 50, 0);
    let maxpos = this._rowColToPosition(cursor.row + 50, Infinity);
    let code = this.getCode();
    let hl = [];
    let ctx = this._isearchContext;
    let point = this.point();
    let count = 0;
    let crnt = 0;
    qrx.lastIndex = ctx.region?.begin || 0;
    while (true) {
        let m = qrx.exec(code);
        if (m && m[0].length) {
            if (ctx.region && qrx.lastIndex > ctx.region.end) {
                break;
            }
            count++;
            if (m.index >= minpos && m.index <= maxpos) {
                let p1 = this._positionToRowCol(m.index);
                let p2 = this._positionToRowCol(qrx.lastIndex);
                hl.push({
                    line1: p1.row, col1: p1.col,
                    line2: p2.row, col2: p2.col
                });
            }
            if (point >= m.index && point < qrx.lastIndex || (point == qrx.lastIndex && ctx.forward)) {
                crnt = count;
            }
        } else {
            break;
        }
    }
    resetPrompt.call(this, count, crnt, prompt);
    this.setOverlay("isearch-lazy", hl);
}

function resetPrompt(count, crnt, prompt) {
    let ctx = this._isearchContext;
    this.whenMinibuffer(mb => {
        if (prompt) {
            mb.prompt(prompt.call(this, count, crnt, ctx));
        } else {
            let pos = ctx.qreplace && count ? `[${count}] `
                : (count && crnt) ? `[${crnt}/${count}] `
                : "";
            mb.prompt(`${pos}${
  ctx.qreplace ? "Query replace" : "I-search"}${
  ctx.regexp ? " regexp" : ctx.word ? " word" : ""}${
  ctx.forward ? "" : " backward"}:`);
        }
        let start = ctx.mbMark;
        let mid = start + ctx.lastFoundQuery?.length;
        if (mid != null) {
            mb.forEachLine((line, c1, c2) => {
                mb._textProperties.removeLineProps(line, c1, c2, "css");
            }, start, mid);
            mb.forEachLine((line, c1, c2) => {
                mb._textProperties.addLineProps(line, c1, c2, "css", "isearch-fail");
            }, mid);
        }
    });
}

function doSearch({ forward } = this._isearchContext) {
    let ctx = this._isearchContext;
    let query = ctx.query;
    let found = false;
    try {
        let rx = searchRegExp.call(this);
        found = this.cmd(forward ? "search_forward_regexp" : "search_backward_regexp", rx);
        if (found) {
            ctx.lastFoundQuery = query;
            this.cmd("ensure_caret_visible");
            let p1 = this.point();
            let p2 = p1 + (forward ? -1 : 1) * this.matchData[0].length;
            if (forward) [ p1, p2 ] = [ p2, p1 ];
            ctx.current = { begin: p1, end: p2 };
            let p1rc = this._positionToRowCol(p1);
            let p2rc = this._positionToRowCol(p2);
            this.setOverlay("isearch", {
                line1: p1rc.row, col1: p1rc.col,
                line2: p2rc.row, col2: p2rc.col,
            });
        }
        lazyHighlight.call(this, rx);
    } catch {};
    return found;
}

function searchRegExp({
    query = null,
    regexp = false,
    lax = true,
    case_fold = true,
    word = false,
} = (this._isearchContext || {})) {
    let searchRX = query;
    if (!regexp) {
        searchRX = query.replace(/[\]\[\}\{\)\(\*\+\?\.\\\^\$\|]/g, "\\$&");
        if (word) {
            // XXX: I guess it would be nice to use syntax_word / syntax_word_dabbrev
            searchRX = "\\b" + searchRX + "\\b";
        }
    }
    if (lax) {
        searchRX = searchRX.replace(/\s+/g, "\\s+");
    }
    return new RegExp(searchRX, caseFold.call(this) ? "ugi" : "ug");
}

Ymacs_Buffer.newCommands({

    isearch_forward: Ymacs_Interactive(function() {
        let arg = { forward: true };
        if (!initIsearch.call(this, arg)) {
            if (!updateIsearch.call(this, arg))
                this.signalError("No more forward occurrences of the search text");
        }
    }),

    isearch_backward: Ymacs_Interactive(function() {
        let arg = { forward: false };
        if (!initIsearch.call(this, arg)) {
            if (!updateIsearch.call(this, arg)) {
                this.signalError("No more backward occurrences of the search text");
            }
        }
    }),

    isearch_forward_regexp: Ymacs_Interactive(function() {
        let arg = { forward: true, regexp: true };
        if (!initIsearch.call(this, arg)) {
            if (!updateIsearch.call(this, arg))
                this.signalError("No more forward occurrences of the search text");
        }
    }),

    isearch_backward_regexp: Ymacs_Interactive(function() {
        let arg = { forward: false, regexp: true };
        if (!initIsearch.call(this, arg)) {
            if (!updateIsearch.call(this, arg))
                this.signalError("No more forward occurrences of the search text");
        }
    }),

    isearch_yank_word_or_char: Ymacs_Interactive(function() {
        if (!this._isearchContext) {
            initIsearch.call(this, { forward: true });
        }
        let ctx = this._isearchContext;
        var pos = ctx.current.end;
        var pos2 = this.cmd("save_excursion", function(){
            this.cmd("goto_char", pos);
            this.cmd("forward_word");
            return this.point();
        });
        if (pos2 != pos) {
            var word = this._bufferSubstring(pos, pos2);
            this.getMinibuffer()._placeUndoBoundary();
            this.getMinibuffer().cmd("insert", word.toLowerCase());
            word = isearchText.call(this);
            this.cmd("goto_char", ctx.current.begin);
            doSearch.call(this, { forward: true });
        }
    }),

    isearch_toggle_word: Ymacs_Interactive(function(){
        let ctx = this._isearchContext;
        ctx.word = !ctx.word;
        if (ctx.word) ctx.regexp = false;
        this.signalInfo(`Search word: ${ctx.word ? "ON" : "OFF"}`, false, 2000);
        this.cmd("goto_char", this._isearchContext.current.begin);
        doSearch.call(this);
    }),

    isearch_toggle_case_fold: Ymacs_Interactive(function(){
        let ctx = this._isearchContext;
        if (ctx.case_fold == null) ctx.case_fold = false;
        else if (ctx.case_fold === false) ctx.case_fold = true;
        else if (ctx.case_fold === true) ctx.case_fold = null;
        this.signalInfo(`Case sensitive: ${ctx.case_fold == null ? "AUTO" : ctx.case_fold ? "OFF" : "ON"}`, false, 2000);
        this.cmd("goto_char", this._isearchContext.current.begin);
        doSearch.call(this);
    }),

    isearch_toggle_lax_whitespace: Ymacs_Interactive(function(){
        let ctx = this._isearchContext;
        ctx.lax = !ctx.lax;
        this.signalInfo(`Loose whitespace: ${ctx.lax ? "ON" : "OFF"}`, false, 2000);
        this.cmd("goto_char", this._isearchContext.current.begin);
        doSearch.call(this);
    }),

    isearch_printing_char: Ymacs_Interactive(function() {
        let ctx = this._isearchContext;
        var ev = this.interactiveEvent();
        if (ev?.key?.length == 1 && !ev.ctrlKey && !ev.altKey) {
            this.whenMinibuffer(mb => {
                mb.cmd("self_insert_command");
                this.cmd("goto_char", ctx.livepoint);
                isearchText.call(this);
                doSearch.call(this);
            });
            return true;
        } else {
            this.cmd("isearch_abort");
            return false;
        }
    }),

    isearch_abort: Ymacs_Interactive(function(cancelled) {
        if (!cancelled)
            this.setGlobal("isearch_last_context", this._isearchContext);
        this.setMinibuffer("");
        this.popKeymap(Ymacs_Keymap_ISearch);
        if (cancelled) {
            this.cmd("goto_char", this._isearchContext.origpoint);
        } else {
            this.markMarker.setPosition(this._isearchContext.origpoint);
        }
        this._isearchContext = null;
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
                ctx.query = ctx.lastFoundQuery;
                doSearch.call(this);
            });
        }
    }),
});

/* -----[ query-replace ]----- */

let Ymacs_Keymap_Query_Replace = Ymacs_Keymap.define("query_replace", {
    "y && Space"               : "query_replace_yes_this_occurrence",
    "."                        : "query_replace_yes_this_occurrence_and_stop",
    "n && Delete && Backspace" : "query_replace_no_this_occurrence",
    "u"                        : "query_replace_undo_previous",
    "!"                        : "query_replace_yes_all_occurrences",
    "q && Enter"               : [ "query_replace_abort", true ],
    "C-l"                      : "recenter_top_bottom",
});
Ymacs_Keymap_Query_Replace.defaultHandler = [ "query_replace_abort" ];

function query_replace_1(args = {}) {
    this.whenMinibuffer(mb => {
        initIsearch.call(this, { ...args, qreplace: true });
        let ctx = this._isearchContext;
        this.cmd("minibuffer_prompt", `Query replace${ctx.regexp ? " regexp" : ctx.word ? " word" : ""}: `);
        let hlOrig = () => {
            isearchText.call(this);
            try {
                lazyHighlight.call(this, searchRegExp.call(this));
            } catch {};
        };
        mb.addEventListener("afterInteractiveCommand", hlOrig);
        let onQuit = (continued) => {
            mb.removeEventListener("afterInteractiveCommand", hlOrig);
            mb.removeEventListener("abort", onQuit);
            if (!continued) {
                this.deleteOverlay("isearch");
                this.deleteOverlay("isearch-lazy");
                this._isearchContext = null;
            }
        };
        mb.addEventListener("abort", onQuit);
        this.cmd("minibuffer_read_string", null, orig => {
            this._isearchContext.query = orig;
            query_replace_2.call(this);
            onQuit(true);
        }, (buf, query, cont) => {
            ctx.query = query;
            try {
                lazyHighlight.call(this, searchRegExp.call(this));
                cont(query);
            } catch {
                buf.popupMessage({ type: "error", text: "Incomplete regexp", atCaret: true });
            }
        });
    });
}

function query_replace_2() {
    let mb = this.getMinibuffer();
    let ctx = this._isearchContext;
    let query = ctx.query;
    let rxorig = searchRegExp.call(this);
    lazyHighlight.call(this, rxorig);
    if (rxorig) {
        this.cmd("minibuffer_prompt", `Replace ${ctx.regexp ? "regexp " : ""}“${query}” with: `);
        let onQuit = () => {
            this.deleteOverlay("isearch");
            this.deleteOverlay("isearch-lazy");
            this._isearchContext = null;
            mb.removeEventListener("abort", onQuit);
        };
        mb.addEventListener("abort", onQuit);
        this.cmd("minibuffer_read_string", null, replacement => {
            this.clearTransientMark();
            query_replace_3.call(this, mb, rxorig, replacement);
        });
    }
}

function similarCase(rplc) {
    let ctx = this._isearchContext;
    if (ctx.case_replace && rplc == rplc.toLowerCase()) {
        let orig = this.matchData[0];
        if (orig == orig.toUpperCase()) return rplc.toUpperCase();
        if (orig == this.capitalize(orig)) return this.capitalize(rplc);
    }
    return rplc;
}

function query_replace_3(mb, rxorig, replacement) {
    let ctx = this._isearchContext;
    this.pushKeymap(Ymacs_Keymap_Query_Replace);
    let cmds, stop, curr, count = 0;
    let queue = [];
    let end = ctx.region && this.createMarker(ctx.region.end);
    this.cmd("goto_char", ctx.region ? ctx.region.begin : ctx.current.begin);

    let domatch = replacement => {
        if (ctx.regexp) {
            // look ma, I can write code like this.
            replacement = replacement.replace(
                /(?:\\([\\\&\#]|\d+|<.+?>))/g,
                (s, p) => p == "\\" ? "\\"
                    :     p == "#" ? count
                    :     p == "&" ? this.matchData[0]
                    :     p[0] == "<" ? this.matchData.groups[p.substr(1, p.length - 2)]
                    : this.matchData[+p]
            );
        }
        return similarCase.call(this, replacement);
    };

    let gotoNext = (all) => {
        let point = this.point();
        let found = this.cmd("search_forward_regexp", rxorig);
        if (found) {
            let m = this.matchData;
            if (ctx.region && (m.index < ctx.region.begin || m.after > end)) {
                if (!all) {
                    this.cmd("goto_char", point);
                }
                return null;
            }
            let begin = this.matchData.index;
            found = { begin: begin, end: this.point() };
            if (!all) {
                this.cmd("ensure_caret_visible");
                let rc_begin = this._positionToRowCol(begin);
                this.setOverlay("isearch", {
                    line1: rc_begin.row, col1: rc_begin.col,
                    line2: this._rowcol.row, col2: this._rowcol.col
                });
            }
        }
        if (!all) {
            lazyHighlight.call(this, rxorig, (count, crnt) =>
                `[${count}] Replace with “${domatch(replacement)}”? (y/n/u/!)`);
        }
        return found;
    };

    cmds = this.replaceCommands({
        query_replace_yes_this_occurrence: Ymacs_Interactive(() => {
            queue.push(curr);
            this._replaceText(curr.begin, curr.end, domatch(replacement));
            count++;
            curr = gotoNext();
            if (!curr) stop();
        }),
        query_replace_yes_this_occurrence_and_stop: Ymacs_Interactive(() => {
            queue.push(curr);
            this._replaceText(curr.begin, curr.end, domatch(replacement));
            count++;
            stop();
        }),
        query_replace_no_this_occurrence: Ymacs_Interactive(() => {
            curr = gotoNext();
            if (!curr) stop();
        }),
        query_replace_yes_all_occurrences: Ymacs_Interactive(() => {
            let last;
            while (curr) {
                last = curr;
                this._replaceText(curr.begin, curr.end, domatch(replacement));
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
        if (end) end.destroy();
        mb.removeEventListener("abort", stop);
        this.newCommands(cmds);
        this.deleteOverlay("isearch");
        this.deleteOverlay("isearch-lazy");
        this._isearchContext = null;
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

function query_replace(regexp) {
    return Ymacs_Interactive("^P", function(word){
        let ctx = this._isearchContext;
        let args = {
            qreplace: true,
            regexp: regexp == null && ctx ? ctx.regexp : !!regexp,
            word: ctx ? ctx.word : !!word,
            region: this.transientMarker && this.getRegion(),
        };
        if (ctx) {
            this.cmd("isearch_abort");
            this._isearchContext = Object.assign({}, ctx, args);
            query_replace_2.call(this);
        } else {
            query_replace_1.call(this, args);
        }
    });
}

Ymacs_Buffer.newCommands({
    query_replace: query_replace(null),
    query_replace_regexp: query_replace(true),
});
