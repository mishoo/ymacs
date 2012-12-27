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

// @require ymacs.js

window.Ymacs_Regexp = (function(){

    var SEARCH_BACKWARD = {};

    // var LOOKING_BACK = {};

    function getPatternAndFlags(rx) {
        if (rx instanceof RegExp)
            rx = rx.toString();
        var pos = rx.lastIndexOf("/"), flags = "";
        flags = rx.substr(pos + 1);
        rx = rx.substring(1, pos);
        return { pattern: rx, flags: flags };
    };

    return {

        // Prepends to the regexp a token that will greedily eat any characters in
        // front of the pattern.  This is useful to get the last occurrence of the
        // pattern in a string.  The returned regexp is cached, so it's not
        // reconstructed a second time.
        //
        // A function using such regexp must be aware that m.index will always be zero,
        // because it matches from the beginning of the string.  To find the index of
        // the real match, it should use m[0].length (where m is an array returned by
        // rx.exec()).
        search_backward: function(rx) {
            var key = rx.toString();
            var cached = SEARCH_BACKWARD[key];
            if (!cached) {
                rx = getPatternAndFlags(key);
                rx.flags = rx.flags.replace(/g/g, "") + "g"; // make sure it's global
                cached = new RegExp("([^]*)(" + rx.pattern + ")", rx.flags);
                SEARCH_BACKWARD[key] = cached;
            }
            cached.lastIndex = 0;
            return cached;
        }

        // Returns a regexp that has the "$" appended, so that it would match only at
        // the end of the string.  This should be faster than using search_backward and
        // checking the lastIndex, since the JS regexp engine can optimize it, knowing
        // that it should match only at the end of the string.  Well, I hope.
        //
        // Update: it's not faster, and it's buggy.  Don't use this.
        //
        // looking_back: function(rx) {
        //         var key = rx.toString();
        //         var cached = LOOKING_BACK[key];
        //         if (!cached) {
        //                 rx = getPatternAndFlags(key);
        //                 rx.pattern = rx.pattern.replace(/\$*$/, "$");
        //                 cached = new RegExp(rx.pattern, rx.flags);
        //                 LOOKING_BACK[key] = cached;
        //         }
        //         return cached;
        // }

    };

})();
