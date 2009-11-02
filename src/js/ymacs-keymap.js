// @require ymacs-buffer.js

DEFINE_CLASS("Ymacs_Keymap", null, function(D, P){

        var REVERSE_KEYS = {};
        Object.foreach(DlKeyboard, function(val, key) {
                if (typeof val == "number")
                        REVERSE_KEYS[val] = key;
        });

        D.DEFAULT_ARGS = {
                definitions : [ "definitions" , null ],
                buffer      : [ "buffer"      , null ]
        };

        D.FIXARGS = function(args) {
                if (!args.definitions)
                        args.definitions = {};
        };

        D.CONSTRUCT = function() {
                this.defaultHandler = this.makeHandler(this.buffer.COMMANDS["self_insert_command"], "self_insert_command");
                this.currentPrefix = null;
                this.currentKeys = [];
        };

        P.parseKey = function(str) {
                var key = {};
                var a = str.split(/-/);
                a.reverse();
                a.foreach(function(c, i){
                        if (i == 0) {
                                if (typeof DlKeyboard[c] == "number")
                                        key.keyCode = DlKeyboard[c];
                                else {
                                        a[i] = c.toLowerCase();
                                        key.charCode = a[i].charCodeAt(0);
                                }
                        } else switch(c) {
                            case "C": key.ctrlKey = true; break;
                            case "M": key.metaKey = true; break;
                            case "S": key.shiftKey = true; break;
                        }
                });
                a.reverse();
                var c = a.pop();
                key.str = a.sort().join("-");
                if (key.str)
                        key.str += "-";
                key.str += c;
                return key;
        };

        P.unparseKey = function(ev) {
                var key, modifiers = [];
                if (ev.keyCode in REVERSE_KEYS)
                        key = REVERSE_KEYS[ev.keyCode];
                else if (ev.charCode) {
                        if (ev.charCode == 32)
                                key = "SPACE";
                        else
                                key = String.fromCharCode(ev.charCode).toLowerCase();
                }
                if (ev.ctrlKey)
                        modifiers.push("C");
                if (ev.altKey)
                        modifiers.push("M");
                if (ev.shiftKey && (ev.charCode && /^[a-zA-Z0-9]$/.test(key) || ev.keyCode))
                        modifiers.push("S");
                modifiers.sort();
                modifiers = modifiers.join("-");
                if (modifiers)
                        modifiers += "-";
                return modifiers + key;
        };

        P.defineKey = function(key, func, args) {
                var cmd = func;
                if (func instanceof Array) {
                        func = func[0];
                        args = func.slice(1);
                }
                key = key.split(/\s*&&\s*/);
                if (key.length > 1) {
                        key.foreach(function(key){
                                this.defineKey(key, func, args);
                        }, this);
                } else {
                        if (typeof func == "string")
                                func = this.buffer.COMMANDS[func];
                        key = key[0].trim();
                        var dfn = this.definitions;
                        if (key.indexOf(" ") >= 0) {
                                var a = key.split(/\s+/);
                                key = a.pop();
                                a.foreach(function(key){
                                        key = this.parseKey(key).str;
                                        if (!dfn[key])
                                                dfn[key] = {};
                                        dfn = dfn[key];
                                }, this);
                        }
                        key = this.parseKey(key);
                        dfn[key.str] = this.makeHandler(func, cmd, args);
                }
        };

        P.defineKeys = function(map) {
                Object.foreach(map, function(func, key){
                        this.defineKey(key, func);
                }, this);
        };

        P.makeHandler = function(func, cmd, args) {
                return this.buffer.makeInteractiveHandler(func, cmd, args);
        };

        P.handleKeyEvent = function(ev) {
                var key = this.unparseKey(ev),
                    def = ( this.currentPrefix
                            ? this.currentPrefix[key]
                            : this.definitions[key] );
                this.currentKeys.push(key);
                if (def instanceof Function) {
                        this.currentPrefix = null;
                        this.currentKeys = [];
                        def();
                        return true;
                }
                if (this.currentPrefix && !def) {
                        this.currentPrefix = null;
                        this.buffer.signalError(this.currentKeys.join(" ") + " is undefined");
                        this.currentKeys = [];
                        return true;
                }
                this.currentPrefix = def;
                if (!def)
                        def = this.defaultHandler();
                return def;
        };

});

DEFINE_CLASS("Ymacs_Keymap_Emacs", Ymacs_Keymap, function(D, P){

        D.KEYS = {
                // movement
                "ARROW_UP && C-p"                         : "backward_line",
                "ARROW_DOWN && C-n"                       : "forward_line",
                "ARROW_LEFT && C-b"                       : "backward_char",
                "ARROW_RIGHT && C-f"                      : "forward_char",
                "HOME"                                    : "beginning_of_indentation_or_line",
                "END && C-e"                              : "end_of_line",
                "C-a"                                     : "beginning_of_line",
                "C-HOME && M-<"                           : "beginning_of_buffer",
                "C-END && M->"                            : "end_of_buffer",
                "C-ARROW_RIGHT && M-f"                    : "forward_word",
                "C-ARROW_LEFT && M-b"                     : "backward_word",
                "C-ARROW_DOWN"                            : "forward_paragraph",
                "C-ARROW_UP"                              : "backward_paragraph",
                "C-l"                                     : "recenter_top_bottom",
                "PAGE_UP && M-v"                          : "scroll_up",
                "PAGE_DOWN && C-v"                        : "scroll_down",

                // basic editing
                "BACKSPACE"                               : "backward_delete_char",
                "DELETE && C-d"                           : "delete_char",
                "ENTER && C-m"                            : "newline",
                "M-d"                                     : "kill_word",
                "C-BACKSPACE && M-BACKSPACE && M-DELETE"  : "backward_kill_word",
                "C-k"                                     : "kill_line",
                "C-y"                                     : "yank",
                "M-y"                                     : "yank_pop",
                "C-SPACE"                                 : "set_mark_command",
                "C-x C-x"                                 : "exchange_point_and_mark",
                "C-w"                                     : "kill_region",
                "M-t"                                     : "transpose_words",
                "C-t"                                     : "transpose_chars",
                "C-x C-t"                                 : "transpose_lines",
                "M-w"                                     : "copy_region_as_kill",
                "M-c"                                     : "capitalize_word",
                "M-u"                                     : "upcase_word",
                "M-l"                                     : "downcase_word",
                "F11"                                     : "nuke_trailing_whitespace",
                "TAB"                                     : "indent_line",
                "M-q"                                     : "fill_paragraph",
                "C-/ && C-x u && C-_ && C-z"              : "undo",
                "INSERT"                                  : "overwrite_mode",
                "M-s"                                     : "center_line",
                "M-/"                                     : "dabbrev_expand",
                "C-s"                                     : "isearch_forward",
                "C-r"                                     : "isearch_backward",
                "M-C-s"                                   : "isearch_forward_regexp",
                "M-C-r"                                   : "isearch_backward_regexp",

                // necessary evil
                "C-S-y"                                   : "yank_from_operating_system",
                "M-S-w"                                   : "copy_for_operating_system",
                "C-S-w"                                   : "kill_for_operating_system",

                // my stuff, sorry if these have different meanings in the standard Emacs keys
                "M-C-d"                                   : "delete_region_or_line",
                "M-S-y"                                   : "yank_shift", // that's the reverse of yank_shift
                "C-c /"                                   : "close_last_xml_tag",
                "S-BACKSPACE"                             : "backward_delete_whitespace",
                "S-DELETE"                                : "delete_whitespace",

                // DEBUG
                "C-x =": function() {
                        alert(this.point());
                }
        };

        D.CONSTRUCT = function() {
                this.defineKeys(D.KEYS);
        };

});
