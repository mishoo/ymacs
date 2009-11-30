// This file is part of Ymacs, an extensible source code editor
// (c) Mihai Bazon 2009 <mihai.bazon@gmail.com>
// Distributed under a BSD-style license.
// http://www.ymacs.org/

// @require ymacs-buffer.js

Ymacs_Buffer.newMode("minibuffer_mode", function(){
        var marker = this.createMarker(0, true);
        var changed_vars = this.setq({ minibuffer_end_marker: marker });
        return function() {
                this.setq(changed_vars);
                marker.destroy();
        };
});

Ymacs_Buffer.newCommands({

        minibuffer_prompt: function(prompt, nofocus) {
                var mb = this.getMinibuffer(), f = this.getMinibufferFrame();
                mb.setCode("");
                mb.cmd("insert", prompt);
                mb.getq("minibuffer_end_marker").setPosition(mb.point());
                f._redrawCaret(true);
                if (!nofocus)
                        f.focus();
        },

        minibuffer_prompt_end: function() {
                var mb = this.getMinibuffer();
                return mb.getq("minibuffer_end_marker").getPosition();
        },

        minibuffer_contents: function() {
                var mb = this.getMinibuffer();
                return mb._bufferSubstring(mb.getq("minibuffer_end_marker"));
        }

});
