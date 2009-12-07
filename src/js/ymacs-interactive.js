// This file is part of Ymacs, an extensible source code editor
// (c) Mihai Bazon 2009 <mihai.bazon@gmail.com>
// Distributed under a BSD-style license.
// http://www.ymacs.org/

(function(){

        /*
         * This is a wrapper that makes it easy to define "interactive" commands.  Pass two arguments: arguments
         * description (args), and a function (func).  args can be null, or a string, an array or a function.  When null
         * it is assumed that the function should not receive any arguments.  When an array or a string, it contains
         * some argument descriptions similar to Emacs:
         *
         *    http://www.gnu.org/s/emacs/manual/html_node/elisp/Interactive-Codes.html#Interactive-Codes
         *
         * (note that Emacs does not take a list for this argument).
         *
         * Ymacs_Interactive returns func.  When not called interactively, the code should supply all the required
         * arguments and the function is called with no performance penalty.  To call it interactively, use
         * func.ymacsCallInteractively(), which will read arguments from the minibuffer according to their description.
         */

        window.Ymacs_Interactive = function(args, func) {
                var documentation;
                if (!(func instanceof Function)) {
                        documentation = func;
                        func = arguments[2];
                        func.ymacsDoc = documentation;
                }
                func.ymacsInteractive = true;
                if (args instanceof Function) {
                        func.ymacsGetArgs = args;
                }
                else if (args != null) {
                        if (!(args instanceof Array))
                                args = args.split(/\n+/);
                        var collect;
                        var execute = function(arg) {
                                collect.push(arg);
                                return func.apply(this, collect);
                        };
                        while (args.length > 0) {
                                execute = createArgumentFunction(args.pop(), function(next, arg) {
                                        collect.push(arg);
                                        next.call(this);
                                }.$(null, execute));
                        }
                        func.ymacsCallInteractively = function(){
                                collect = [];
                                return execute.call(this);
                        };
                } else {
                        func.ymacsCallInteractively = func;
                }
                return func;
        };

        /* -----[ argument reader functions ]----- */

        function read_function_name(arg, cont) {

        };

        function read_existing_buffer_name(arg, cont) {

        };

        function read_buffer_name(arg, cont) {

        };

        function read_character(arg, cont) {

        };

        function read_command_name(arg, cont) {
                this.cmd("minibuffer_prompt", arg);
                this.cmd("minibuffer_read_command", cont);
        };

        function get_point(arg, cont) {
                cont.call(this, this.point());
        };

        function get_mouse_event(arg, cont) {

        };

        function irrelevant(arg, cont) {
                cont.call(this, null);
        };

        function read_key_sequence(arg, cont) {

        };

        function read_key_sequence2(arg, cont) {

        };

        function get_mark(arg, cont) {
                cont.call(this, this.markMarker.getPosition());
        };

        function read_arbitrary_text(arg, cont) {

        };

        function read_number(arg, cont) {

        };

        function read_number_or_prefix(arg, cont) {

        };

        function get_numeric_prefix(arg, cont) {

        };

        function get_raw_prefix(arg, cont) {

        };

        function get_point_and_mark(arg, cont) {
                cont.call(this, this.getRegion());
        };

        function read_key_sequence3(arg, cont) {

        };

        function read_variable_name(arg, cont) {

        };

        var ARG_READERS = {
                a: read_function_name,
                b: read_existing_buffer_name,
                B: read_buffer_name,
                c: read_character,
                C: read_command_name,
                d: get_point,
                e: get_mouse_event,
                i: irrelevant,
                k: read_key_sequence,
                K: read_key_sequence2,
                m: get_mark,
                M: read_arbitrary_text,
                n: read_number,
                N: read_number_or_prefix,
                p: get_numeric_prefix,
                P: get_raw_prefix,
                r: get_point_and_mark,
                s: read_arbitrary_text,
                U: read_key_sequence3,
                v: read_variable_name

                // D: no directory name reader in Ymacs
                // f:
                // F:
                // G: no file name reader in Ymacs
                // S: no reader for interned symbols in Ymacs
                // no x, X, z and Z either
        };

        function createArgumentFunction(arg, cont) {
                var code = arg.charAt(0);
                arg = arg.substr(1);
                return ARG_READERS[code].$(null, arg, cont);
        };

})();
