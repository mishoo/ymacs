// This file is part of Ymacs, an extensible source code editor
// (c) Mihai Bazon 2009 <mihai.bazon@gmail.com>
// Distributed under a BSD-style license.
// http://www.ymacs.org/

// @require ymacs-buffer.js

EXTEND_CLASS(Ymacs_Buffer, function(D, P){

        function read_function_name(cont) {

        };

        function read_existing_buffer_name(cont) {

        };

        function read_buffer_name(cont) {

        };

        function read_character(cont) {

        };

        function read_command_name(cont) {

        };

        function get_point(cont) {
                cont(this.point());
        };

        function get_mouse_event(cont) {

        };

        function irrelevant(cont) {
                cont(null);
        };

        function read_key_sequence(cont) {

        };

        function read_key_sequence2(cont) {

        };

        function get_mark(cont) {
                cont(this.markMarker.getPosition());
        };

        function read_arbitrary_text(cont) {

        };

        function read_number(cont) {

        };

        function read_number_or_prefix(cont) {

        };

        function get_numeric_prefix(cont) {

        };

        function get_raw_prefix(cont) {

        };

        function get_point_and_mark(cont) {
                return this.getRegion();
        };

        function read_key_sequence3(cont) {

        };

        function read_variable_name(cont) {

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

        function createArgumentFunction(arg) {
                var code = arg.charAt(0);
                arg = arg.substr(1);
        };

        D.newInteractiveCommand = function(cmd, args, func){
                if (args instanceof Function) {

                }
                else {
                        if (!(args instanceof Array))
                                args = args.trim().split(/\n+/);
                        args = args.map(createArgumentFunction);
                }
        };

});

Ymacs_Buffer.newCommands({

        completing_read: function(prompt, collection) {
        }

});
