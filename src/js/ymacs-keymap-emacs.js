/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT

import { Ymacs_Keymap } from "./ymacs-keymap.js";
import { Ymacs_Interactive } from "./ymacs-interactive.js";

// This is the default keymap, as configured in
// Ymacs_Buffer::makeDefaultKeymap.  It follows closely the standard
// Emacs keybindings, with some small deviations that match my taste
// (search for "my stuff" below).

// It would be nice to have more options, such as an Eclipse keymap,
// or Visual Studio -- but I'm not familiar with any of them.
// Contributions welcome.

// A keymap inherits from Ymacs_Keymap and should define its bindings
// in D.KEYS.  They can also define a defaultHandler property to take
// whatever action they consider necessary -- for example the Isearch
// keymap will, by default, print the entered character in the
// minibuffer and trigger a search action.  Isearch mode is almost
// completely defined in a keymap -- with the minor note that
// isearch_forward and isearch_backward are assigned below to key
// combinations; once pressed, they will push the Isearch keymap onto
// the buffer's keymap stack, and it will be used until isearch is
// ended.

/* KEY DEFINITIONS.  A key is generally defined in standard Emacs
   notation, with the following notes:

   - it is possible to define multiple keys at once for the same
     operation, by using the "&&" combination (separate with spaces
     from the actual keys)

   - some special characters are named literally (see event.key
     https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key)

   Normally you would define commands using Ymacs_Buffer.newCommands
   (see ymacs-commands.js) and specify the command name for key
   bindings, but you can directly use a function as well:

   "C-8": function() {
     alert("You pressed CTRL-8");
   }

   It is advised to define commands for more than simple cases, for
   two reasons:

   1. they can be used for "non-interactive" calls too

   2. the command name is saved in this.currentCommand /
      this.previousCommand, which is useful in a number of cases.

*/

let minibuffer_keys = {
    // movement
    "ArrowLeft   && C-b"                      : "backward_char",
    "ArrowRight  && C-f"                      : "forward_char",
    "Home"                                    : "beginning_of_indentation_or_line",
    "End && C-e"                              : "end_of_line",
    "C-a"                                     : "beginning_of_line",
    "C-Home && M-<"                           : "beginning_of_buffer",
    "C-End && M->"                            : "end_of_buffer",
    "C-ArrowRight && M-f"                     : "forward_word",
    "C-ArrowLeft && M-b"                      : "backward_word",

    // transient mark
    "S-ArrowLeft     && S-C-b"                : "backward_char_mark",
    "S-ArrowRight    && S-C-f"                : "forward_char_mark",
    "S-C-ArrowRight  && S-M-f"                : "forward_word_mark",
    "S-C-ArrowLeft   && S-M-b"                : "backward_word_mark",
    "S-Home"                                  : "beginning_of_indentation_or_line_mark",
    "S-C-a"                                   : "beginning_of_line_mark",
    "S-End && S-C-e"                          : "end_of_line_mark",
    "S-C-Home"                                : "beginning_of_buffer_mark",
    "S-C-End"                                 : "end_of_buffer_mark",

    // basic editing
    "Backspace"                               : "backward_delete_char",
    "Delete && C-d"                           : "delete_char",
    "M-d && C-Delete"                         : "kill_word",
    "C-Backspace && M-Backspace && M-Delete"  : "backward_kill_word",
    "C-k"                                     : "kill_line",
    "C-y && S-Insert"                         : "yank",
    "M-y"                                     : "yank_pop",
    "C-Space"                                 : "set_mark_command",
    "C-x C-x"                                 : "exchange_point_and_mark",
    "C-w"                                     : "kill_region",
    "M-t"                                     : "transpose_words",
    "C-t"                                     : "transpose_chars",
    "M-w"                                     : "copy_region_as_kill",
    "M-c"                                     : "capitalize_word",
    "M-u"                                     : "upcase_word",
    "M-l"                                     : "downcase_word",
    "F11"                                     : "nuke_trailing_whitespace",
    "C-/ && C-x u && C-_ && C-z"              : "undo",
    "Insert"                                  : "overwrite_mode",
    "M-/"                                     : "dabbrev_expand",
    "C-g"                                     : "keyboard_quit",
    "Escape"                                  : "keyboard_quit",

    // "Escape"                                  : "_next_is_meta",
    // "M-Escape Escape"                         : "keyboard_quit",

    "C-S-y && C-v"                            : "yank_from_operating_system",
    "M-S-w"                                   : "copy_for_operating_system",

    "C-c /"                                   : "close_last_xml_tag",
    "S-Backspace"                             : "backward_delete_whitespace",
    "S-Delete"                                : "delete_whitespace",

    "C-x ="                                   : "what_cursor_position",
};

let emacs_keys = Object.assign({}, minibuffer_keys, {
    // movement
    "ArrowUp     && C-p"                      : "backward_line",
    "ArrowDown   && C-n"                      : "forward_line",
    "ArrowLeft   && C-b"                      : "backward_char",
    "ArrowRight  && C-f"                      : "forward_char",
    "Home"                                    : "beginning_of_indentation_or_line",
    "End && C-e"                              : "end_of_line",
    "C-a"                                     : "beginning_of_line",
    "C-Home && M-<"                           : "beginning_of_buffer",
    "C-End && M->"                            : "end_of_buffer",
    "C-ArrowRight && M-f"                     : "forward_word",
    "C-ArrowLeft && M-b"                      : "backward_word",
    "C-ArrowDown"                             : "forward_paragraph",
    "C-ArrowUp"                               : "backward_paragraph",
    "M-h"                                     : "mark_paragraph",
    "C-l"                                     : "recenter_top_bottom",
    "PageUp"                                  : "scroll_up_half",
    "PageDown"                                : "scroll_down_half",
    "WheelUp"                                 : "scroll_up",
    "WheelDown"                               : "scroll_down",

    // transient mark
    "S-ArrowUp       && S-C-p"                : "backward_line_mark",
    "S-ArrowDown     && S-C-n"                : "forward_line_mark",
    "S-ArrowLeft     && S-C-b"                : "backward_char_mark",
    "S-ArrowRight    && S-C-f"                : "forward_char_mark",
    "S-C-ArrowRight  && S-M-f"                : "forward_word_mark",
    "S-C-ArrowLeft   && S-M-b"                : "backward_word_mark",
    "S-C-ArrowDown"                           : "forward_paragraph_mark",
    "S-C-ArrowUp"                             : "backward_paragraph_mark",
    "S-Home"                                  : "beginning_of_indentation_or_line_mark",
    "S-C-a"                                   : "beginning_of_line_mark",
    "S-End && S-C-e"                          : "end_of_line_mark",
    "S-C-Home"                                : "beginning_of_buffer_mark",
    "S-C-End"                                 : "end_of_buffer_mark",

    // basic editing
    "Backspace"                               : "backward_delete_char",
    "Delete && C-d"                           : "delete_char",
    "Enter && C-m"                            : "newline",
    "M-d && C-Delete"                         : "kill_word",
    "C-Backspace && M-Backspace && M-Delete"  : "backward_kill_word",
    "C-k"                                     : "kill_line",
    "C-y && S-Insert"                         : "yank",
    "M-y"                                     : "yank_pop",
    "C-Space"                                 : "set_mark_command",
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
    "Tab"                                     : "indent_line",
    "C-M-\\"                                  : "indent_region",
    "M-q"                                     : "fill_paragraph",
    "C-/ && C-x u && C-_ && C-z"              : "undo",
    "Insert"                                  : "overwrite_mode",
    "M-s"                                     : "center_line",
    "M-/"                                     : "dabbrev_expand",
    "C-s"                                     : "isearch_forward",
    "C-r"                                     : "isearch_backward",
    "C-S-s"                                   : "isearch_yank_word_or_char",
    "M-C-s"                                   : "isearch_forward_regexp",
    "M-C-r"                                   : "isearch_backward_regexp",
    "M-%"                                     : "query_replace",
    "C-M-%"                                   : "query_replace_regexp",
    "C-u"                                     : "universal_argument",
    "M-g"                                     : "goto_line",
    "C-x h"                                   : "mark_whole_buffer",
    "C-g"                                     : "keyboard_quit",
    "M-^"                                     : "delete_indentation",
    "M-;"                                     : "comment_dwim",
    "C-x ="                                   : "what_cursor_position",
    "C-x f"                                   : "set_fill_column",
    "C-x C-\\"                                : "goto_last_change",

    // vertical editing
    "C-x r t"                                 : "string_rectangle",
    "C-x r c"                                 : "clear_rectangle",
    "C-x r k"                                 : "kill_rectangle",
    "C-x r y"                                 : "yank_rectangle",

    // buffers
    "C-x C-ArrowRight && C-x ArrowRight && C-Tab"    : "next_buffer",
    "C-x C-ArrowLeft && C-x ArrowLeft && C-S-Tab"    : "previous_buffer",
    "C-x b"                                          : "switch_to_buffer",
    "C-x k"                                          : "kill_buffer",

    // frames
    "C-x 0"                                   : "delete_frame",
    "C-x 1"                                   : "delete_other_frames",
    "C-x 2"                                   : "split_frame_vertically",
    "C-x 3"                                   : "split_frame_horizontally",
    "C-x o"                                   : "other_frame",
    "C-x l"                                   : "toggle_line_numbers",

    // eval
    "M-x"                                     : "execute_extended_command",

    "C-S-y && C-v"                            : "yank_from_operating_system",
    "M-S-w"                                   : "copy_for_operating_system",

    // my stuff, sorry if these have different meanings in the standard Emacs keys
    "M-S-y"                                   : "yank_shift", // that's the reverse of yank_shift
    "C-c /"                                   : "close_last_xml_tag",
    "S-Backspace"                             : "backward_delete_whitespace",
    "S-Delete"                                : "delete_whitespace",
    "C-M-d"                                   : "delete_region_or_line",
    "M-Enter"                                 : "start_next_paragraph",
    "C-M-|"                                   : "cperl_lineup",
    "C-F4"                                    : "kill_buffer",
    "M-ArrowLeft"                             : [ "windmove", "left" ],
    "M-ArrowRight"                            : [ "windmove", "right" ],
    "M-ArrowUp"                               : [ "windmove", "up" ],
    "M-ArrowDown"                             : [ "windmove", "down" ],

    "C-x e"                                   : "kmacro_end_and_call_macro",
    "C-x ("                                   : "kmacro_start_macro",
    "C-x )"                                   : "kmacro_end_macro",

    // file system commands
    "C-x C-f"                                 : "find_file",
    "C-x C-w"                                 : "write_file",
    "C-x C-s"                                 : "save_buffer",
    "C-x s"                                   : "save_some_buffers",
});

let minibuffer_keymap = Ymacs_Keymap.define("emacs_mb", minibuffer_keys);
minibuffer_keymap.defaultHandler = [ "self_insert_command" ];

let emacs_keymap = Ymacs_Keymap.define("emacs", emacs_keys);
emacs_keymap.defaultHandler = [ "self_insert_command" ];

let keymap_universal_arg = Ymacs_Keymap.define("universal_arg", {});
keymap_universal_arg.defaultHandler = [ Ymacs_Interactive("^", function(){
    var ev = this.interactiveEvent();
    var ch = ev.key;
    var prefix = this.getPrefixArg(true);
    if ((/^[0-9]$/.test(ch) || (ch === "-" && prefix === "")) && !ev.altKey && !ev.ctrlKey) {
        prefix += ch;
        this.setPrefixArg(prefix);
        if (!this.isMinibuffer) {
            this.whenMinibuffer(function(mb){
                mb.cmd("insert", " ", ch);
            });
        }
        return true;
    }
    this.popKeymap(keymap_universal_arg);
    return false;
}) ];
keymap_universal_arg.attached = buffer => buffer.setPrefixArg("");

export {
    emacs_keymap as Ymacs_Keymap_Emacs,
    minibuffer_keymap as Ymacs_Keymap_Minibuffer,
    keymap_universal_arg as Ymacs_Keymap_UniversalArgument,
};
