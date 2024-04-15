/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT

import { EventProxy, remove } from "./ymacs-utils.js";

/* markers are objects that hold a position which is automatically
   maintained as text is inserted or removed */

export class Ymacs_Marker {

    constructor({
        pos = null,
        editor = null,
        before = false,
        name = null
    } = {}) {
        this.position = pos;
        this.editor = editor;
        this.before = before;
        this.name = name;

        this.editor.markers.push(this);
        this.rowcol = null;
        this.onChange = [];
    };

    destroy() {
        remove(this.editor.markers, this);
        this.editor = null;
    }

    editorChange(pos, diff, min) {
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
    }

    callHooks(a, arg) {
        for (var i = a.length; --i >= 0;)
            a[i].call(this.editor, arg);
    }

    getPosition() {
        return this.position;
    }

    valueOf() {
        return this.position;
    }

    setPosition(pos, noHooks, force) {
        if (force || this.position != pos) {
            this.rowcol = null;
            this.position = pos;
            if (!noHooks)
                this.callHooks(this.onChange, this.position);
        }
    }

    getRowCol() {
        return this.rowcol || (this.rowcol = this.editor._positionToRowCol(this.position));
    }

    swap(other, noHooks, force) {
        var tmp = this.getPosition();
        this.setPosition(other.getPosition(), noHooks, force);
        other.setPosition(tmp, noHooks, force);
    }

}
