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
        };

        P.onChange = Function.noop;

        P.destroy = function() {
                this.editor.markers.remove(this);
                this.editor = null;
        };

        P.editorChange = function(pos, diff, min) {
                var p = this.position;
                if (this.before)
                        --p;
                if (diff != 0 && pos <= p) {
                        this.position += diff;
                        if (this.position < min)
                                this.position = min;
                        this.onChange(this.position);
                }
        };

        P.getPosition = function() {
                return this.position;
        };

        P.setPosition = function(pos, noHooks, force) {
                if (force || this.position != pos) {
                        this.position = pos;
                        if (!noHooks)
                                this.onChange(this.position);
                }
        };

        P.getRowCol = function() {
                return this.editor._positionToRowCol(this.position);
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
