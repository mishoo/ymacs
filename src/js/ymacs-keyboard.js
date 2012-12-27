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

// This file tries to define some keyboard constants based on the
// sheer insanity of browser inconsistencies described here:
//
//     http://unixpapa.com/js/key.html
//
// Kudos for that work!

(function(){

    var letters = {};
    for (var i = 65; i <= 90; ++i)
        letters[i] = [ i, i + 32 ];
    letters[32] = [ 32, 32 ];

    var modifiers = [ 16, 17, 18, 20, 144 ].toHash(true);

    var digit_charcodes = [
        [ 49, 33 ],
        [ 50, 64 ],
        [ 51, 35 ],
        [ 52, 36 ],
        [ 53, 37 ],
        [ 54, 94 ],
        [ 55, 38 ],
        [ 56, 42 ],
        [ 57, 40 ],
        [ 48, 41 ]
    ];

    var digits = [ 49, 50, 51, 52, 53, 54, 55, 56, 57, 48 ].toHash(function(k, i) {
        return digit_charcodes[i];
    });

    var symbol_charcodes = [
        [ 59, 58 ],
        [ 61, 43 ],
        [ 44, 60 ],
        [ 45, 95 ],
        [ 46, 62 ],
        [ 47, 63 ],
        [ 96, 126 ],
        [ 91, 123 ],
        [ 92, 124 ],
        [ 93, 125 ],
        [ 39, 34 ]
    ];

    var symbols = ( is_gecko ? [ 59, 61, 188, 109, 190, 191, 192, 219, 220, 221, 222 ]
                    : is_opera ? [ 59, 61, 44, 45, 46, 47, 96, 91, 92, 93, 39 ]
                    : [ 186, 187, 188, 189, 190, 191, 192, 219, 220, 221, 222 ] ).toHash(function(k, i) {
                        return symbol_charcodes[i];
                    });

    var arrows = [ 37, 38, 39, 40 ].toHash(true);

    var specials = [ 45, 46, 36, 35, 33, 34, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123 ].toHash(true);

    function getCharCode(code, shift) {
        var a = letters[code] || digits[code] || symbols[code];
        return a ? shift ? a[1] : a[0] : null;
    };

    window.KEYBOARD_INSANITY = {
        letters     : letters,
        modifiers   : modifiers,
        digits      : digits,
        symbols     : symbols,
        arrows      : arrows,
        specials    : specials,
        getCharCode : getCharCode
    };

})();
