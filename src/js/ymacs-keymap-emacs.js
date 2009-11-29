// This file is part of Ymacs, an extensible source code editor
// (c) Mihai Bazon 2009 <mihai.bazon@gmail.com>
// Distributed under a BSD-style license.
// http://www.ymacs.org/

// @require ymacs-keymap.js

// This is the default keymap, as configured in
// Ymacs_Buffer::makeDefaultKeymap.  It follows closely the standard
// Emacs keybindings, with some small deviations that match my taste
// (search for "my stuff" below).

// It would be nice to have more options, such as an Eclipse keymap,
// or Visual Studio -- but I'm not familiar with any of them.
// Contributions welcome.

// A keymap inherits from Ymacs_Keymap and should call in its
// constructor this.defineKeys for a hash like the one below.
// Aditionally, for specific keymaps (such as those defined by modes)
// they should set this.defaultHandler = null, so that keys that
// aren't found in the current keymap will be searched in earlier
// defined ones.  Or, to take whatever action they consider necessary
// -- for example the Isearch keymap will, by default, print the
// entered character in the minibuffer and trigger a search action.
// Isearch mode is almost completely defined in a keymap -- with the
// minor note that isearch_forward and isearch_backward are assigned
// below to key combinations; once pressed, they will push the Isearch
// keymap onto the buffer's keymap stack, and it will be used until
// isearch is ended.

/* KEY DEFINITIONS.  A key is generally defined in standard Emacs
   notation, with the following notes:

   - it is possible to define multiple keys at once for the same
     operation, by using the "&&" combination (separate with spaces
     from the actual keys)

   - some special characters are named literally; their names contain
     more than one character.  The available names are defined in
     DlKeyboard (see keyboard.js in DynarchLIB).  Examples:
     ARROW_RIGHT, PAGE_UP, etc. but also SPACE and DASH.

   Normally you would define commands using Ymacs_Buffer.newCommands
   (see ymacs-commands.js) and specify the command name for key
   bindings, but you can specify a function as well, if you want:

     "C-8": function() {
       alert("You pressed CTRL-8");
     }

   It is advised to define commands for more than simple cases, for
   two reasons:

   1. they can be used for "non-interactive" calls too

   2. the command name is saved in this.currentCommand /
      this.previousCommand, which is useful in a number of cases.

*/

DEFINE_CLASS("Ymacs_Keymap_Emacs", Ymacs_Keymap, function(D, P){

        var TMPL_INFO = String.template(
                "<table>",
                "<tr><td style='text-align: right; font-weight: bold'>Char:</td><td><tt> $ch </tt></td></tr>",
                "<tr><td style='text-align: right; font-weight: bold'>Char code:</td><td> $code / 0x$codeHex </td></tr>",
                "<tr><td style='text-align: right; font-weight: bold'>Position:</td><td> $point </td></tr>",
                "<tr><td style='text-align: right; font-weight: bold'>Buffer size:</td><td> $sizeKB </td></tr>",
                "</table>"
        );

        D.KEYS = {
                // movement
                "ARROW_UP     && C-p"                     : "backward_line",
                "ARROW_DOWN   && C-n"                     : "forward_line",
                "ARROW_LEFT   && C-b"                     : "backward_char",
                "ARROW_RIGHT  && C-f"                     : "forward_char",
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

                // transient mark
                "S-ARROW_UP       && S-C-p"               : "backward_line_mark",
                "S-ARROW_DOWN     && S-C-n"               : "forward_line_mark",
                "S-ARROW_LEFT     && S-C-b"               : "backward_char_mark",
                "S-ARROW_RIGHT    && S-C-f"               : "forward_char_mark",
                "S-C-ARROW_RIGHT  && S-M-f"               : "forward_word_mark",
                "S-C-ARROW_LEFT   && S-M-b"               : "backward_word_mark",
                "S-C-ARROW_DOWN"                          : "forward_paragraph_mark",
                "S-C-ARROW_UP"                            : "backward_paragraph_mark",
                "S-HOME"                                  : "beginning_of_indentation_or_line_mark",
                "S-C-a"                                   : "beginning_of_line_mark",
                "S-END"                                   : "end_of_line_mark",
                "S-C-HOME"                                : "beginning_of_buffer_mark",
                "S-C-END"                                 : "end_of_buffer_mark",

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
                "C-M-\\"                                  : "indent_region",
                "M-q"                                     : "fill_paragraph",
                "C-/ && C-x u && C-_ && C-z"              : "undo",
                "INSERT"                                  : "overwrite_mode",
                "M-s"                                     : "center_line",
                "M-/"                                     : "dabbrev_expand",
                "C-s"                                     : "isearch_forward",
                "C-r"                                     : "isearch_backward",
                "M-C-s"                                   : "isearch_forward_regexp",
                "M-C-r"                                   : "isearch_backward_regexp",

                // buffers
                "C-x C-ARROW_RIGHT && C-x ARROW_RIGHT && C-TAB" : "next_buffer",
                "C-x C-ARROW_LEFT && C-x ARROW_LEFT && C-S-TAB" : "previous_buffer",

                // frames
                "C-x 1"                                   : "delete_other_frames",
                "C-x 2"                                   : "split_frame_vertically",
                "C-x 3"                                   : "split_frame_horizontally",
                "C-x o"                                   : "other_frame",

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
                "M-ENTER"                                 : "start_next_paragraph",
                "M-S-q"                                   : "fill_paragraph_no_prefix",

                // others
                "C-x =": function() {
                        var ch = this.charAt(), chname = ch;
                        if (ch == " ")
                                chname = "<SPACE>";
                        else if (ch == "\n")
                                chname = "<NEWLINE>";
                        else if (ch == "-")
                                chname = "<DASH>";
                        this.signalInfo(TMPL_INFO({
                                ch      : chname.htmlEscape(),
                                code    : ch.charCodeAt(0),
                                codeHex : ch.charCodeAt().hex(),
                                point   : this.point(),
                                size    : this.getCodeSize(),
                                sizeKB  : this.getCodeSize().formatBytes(2)
                        }), true);
                }
        };

        D.CONSTRUCT = function() {
                this.defineKeys(D.KEYS);
        };

});
