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

// @require ymacs-buffer.js

/* -----[ markers are objects that hold a position which is
   automatically maintained as text is inserted or
   removed ]----- */

DEFINE_CLASS("Ymacs_Marker", null, function(D, P){

    D.DEFAULT_ARGS = {
        position : [ "pos"    , null ],
        editor   : [ "editor" , null ],
        before   : [ "before" , false ],
        name     : [ "name"   , null ]
    };

    D.CONSTRUCT = function() {
        this.editor.markers.push(this);
        this.rowcol = null;
        this.onChange = [];
    };

    P.destroy = function() {
        this.editor.markers.remove(this);
        this.editor = null;
    };

    P.editorChange = function(pos, diff, min) {
        var p = this.position;
        if (this.before)
            --p;
        if (diff != 0 && pos <= p) {
            this.rowcol = null;
            this.position += diff;
            if (this.position < min)
                this.position = min;
            this.callHooks(this.onChange, this.position);
        }
    };

    P.callHooks = function(a, arg) {
        for (var i = a.length; --i >= 0;)
            a[i].call(this.editor, arg);
    };

    P.getPosition = function() {
        return this.position;
    };

    P.setPosition = function(pos, noHooks, force) {
        if (force || this.position != pos) {
            this.rowcol = null;
            this.position = pos;
            if (!noHooks)
                this.callHooks(this.onChange, this.position);
        }
    };

    P.getRowCol = function() {
        return this.rowcol || (this.rowcol = this.editor._positionToRowCol(this.position));
    };

    P.updateMarkers = function(delta) {
        this.editor._updateMarkers(this.getPosition(), delta);
    };

    P.swap = function(other, noHooks, force) {
        var tmp = this.getPosition();
        this.setPosition(other.getPosition(), noHooks, force);
        other.setPosition(tmp, noHooks, force);
    };

});
