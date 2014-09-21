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

// @require ymacs-exception.js

(function(){

    /*
     * Ymacs_Interactive(args_description, function_reference)
     *
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
        if (arguments.length == 1) {
            func = args;
            args = null;
        } else {
            var documentation;
            if (!(func instanceof Function)) {
                documentation = func;
                func = arguments[2];
                func.ymacsDoc = documentation;
            }
        }
        func.ymacsInteractive = true;
        if (args instanceof Function) {
            func.ymacsGetArgs = args;
        }
        else if (args != null) {
            if (!(args instanceof Array)) {
                var m = /^[\^\@\*]+/.exec(args);
                if (m) {
                    m = m[0];
                    args = args.substr(m.length);
                    if (m.indexOf("^") >= 0) {
                        func.ymacsMarkExtend = true;
                    }
                    if (m.indexOf("*") >= 0) {
                        func.ymacsWarnReadonly = true;
                    }
                    if (m.indexOf("@") >= 0) {
                        func.ymacsSelectFrame = true;
                    }
                }
                if (args)
                    args = args.split(/\n+/);
            }
            if (args) {
                var collect,
                execute = function() {
                    collect.append(Array.$(arguments));
                    return this.callInteractively(func, collect, true);
                };
                while (args.length > 0) {
                    execute = createArgumentFunction(args.pop(), function(next) {
                        collect.append(Array.$(arguments, 1));
                        next.call(this);
                    }.$(null, execute));
                }
                func.ymacsCallInteractively = function(){
                    collect = [];
                    return execute.call(this);
                };
            }
        }
        return func;
    };

    window.Ymacs_Interactive_X = function(func) {
        return Ymacs_Interactive("p", function(n){
            if (n == null) n = 1;
            n.times(func, this);
        });
    };

    var $TRUE = (function(){});
    $TRUE.toString = function() { return "" };
    $TRUE.empty = true;

    /* -----[ argument reader functions ]----- */

    function prompt(arg) {
        var pr = this.getPrefixArg(true /* noDiscard */);
        if (pr) {
            arg = pr + " " + arg;
        }
        this.cmd("minibuffer_prompt", arg);
    };

    function read_function_name(arg, cont) {
        console.log("read_function_name", arg);
        prompt.call(this, arg);
        this.cmd("minibuffer_read_function", cont);
        // XXX: enforce it!
    };

    function read_existing_buffer_name(arg, cont) {
        prompt.call(this, arg);
        this.cmd("minibuffer_read_buffer", cont);
        // XXX: enforce it!
    };

    function read_buffer_name(arg, cont) {
        prompt.call(this, arg);
        this.cmd("minibuffer_read_buffer", cont);
    };

    function read_character(arg, cont) {

    };

    function read_command_name(arg, cont) {
        prompt.call(this, arg);
        this.cmd("minibuffer_read_command", cont);
        // XXX: enforce it!
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
        prompt.call(this, arg);
        this.cmd("minibuffer_read_string", null, cont);
    };

    function read_number(arg, cont) {
        prompt.call(this, arg);
        this.cmd("minibuffer_read_number", cont);
    };

    function read_number_or_prefix(arg, cont) {
        var n = parseInt(this.getPrefixArg(), 10);
        if (!isNaN(n))
            cont.call(this, n);
        else
            read_number.call(this, arg, cont);
    };

    function get_numeric_prefix(arg, cont) {
        var n = parseInt(this.getPrefixArg(), 10);
        if (isNaN(n))
            n = null;
        cont.call(this, n);
    };

    function get_raw_prefix(arg, cont) {
        arg = this.getPrefixArg();
        if (arg === "")
            arg = $TRUE;
        cont.call(this, arg);
    };

    function get_point_and_mark(arg, cont) {
        var r = this.getRegion();
        cont.call(this, r.begin, r.end);
    };

    function read_key_sequence3(arg, cont) {

    };

    function read_variable_name(arg, cont) {
        prompt.call(this, arg);
        this.cmd("minibuffer_read_variable", cont);
    };

    function read_existing_file_name(arg, cont) {
        prompt.call(this, arg);
        this.cmd("minibuffer_read_existing_file", cont);
    };

    function read_file_name(arg, cont) {
        prompt.call(this, arg);
        this.cmd("minibuffer_read_file", cont);
    };

    function read_file_or_directory_name(arg, cont) {
        prompt.call(this, arg);
        this.cmd("minibuffer_read_file_or_directory", cont);
    };

    function read_existing_directory_name(arg, cont) {
        prompt.call(this, arg);
        this.cmd("minibuffer_read_directory", cont);
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
        v: read_variable_name,

        f: read_existing_file_name,
        F: read_file_name,
        G: read_file_or_directory_name,
        D: read_existing_directory_name

        // S: no reader for interned symbols in Ymacs
        // no x, X, z and Z either
    };

    function createArgumentFunction(arg, cont) {
        var code = arg.charAt(0);
        arg = arg.substr(1);
        return ARG_READERS[code].$(null, arg, cont);
    };

})();
