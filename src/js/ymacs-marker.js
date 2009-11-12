// This file is part of Ymacs, an extensible source code editor
// (c) Mihai Bazon 2009 <mihai.bazon@gmail.com>
// Distributed under a BSD-style license.
// http://www.ymacs.org/

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
