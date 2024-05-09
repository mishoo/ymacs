import { Ymacs, Ymacs_Keymap, Ymacs_Interactive, Ymacs_Buffer } from "../src/index.js";

function post(cmd, args) {
    if (args) args = JSON.stringify(args);
    return fetch("/" + cmd, {
        method: "POST",
        headers: {
            "Accept": "application/json",
            "Content-Type": "application/json",
        },
        body: args,
    });
}

Ymacs_Buffer.newCommands({
    dogfood_load_file: Ymacs_Interactive(async function(){
        this.cmd("minibuffer_prompt", "Fetch from Emacs:");
        let buffers = await (await post("ymacs-buffer-list")).json();
        this.cmd("minibuffer_read_string", buffers, async name => {
            let ymacs = this.ymacs;
            let buf = ymacs.getBuffer(name);
            if (!buf) {
                let { code, mode, point } = await (await post(`ymacs-buffer-get/${name}`)).json();
                buf = ymacs.createBuffer({ name, code });
                switch (true) {
                  case /js2?-mode|javascript|json/.test(mode):
                    buf.cmd("javascript_mode");
                    break;
                  case /s?css-mode/.test(mode):
                    buf.cmd("css_mode");
                    break;
                  case /web-mode|html|xml/.test(mode):
                    buf.cmd("html_mode");
                    break;
                  case /lisp/.test(mode):
                    buf.cmd("lisp_mode");
                    break;
                }
                if (point != null) {
                    buf.cmd("goto_char", point - 1);
                }
            }
            ymacs.switchToBuffer(buf);
        }, (mb, name, cont) => {
            if (buffers.includes(name)) {
                cont(name);
            } else {
                mb.popupMessage({
                    type: "error",
                    text: "Buffer not found",
                    atCaret: true,
                    timeout: 2000,
                });
            }
        });
    }),
    dogfood_save_file: Ymacs_Interactive(async function(cont){
        let result = await (await post(`ymacs-buffer-save/${this.name}`, {
            code: this.getCode(),
            point: this.point() + 1,
        })).json();
        if (result.ok == this.name) {
            this.dirty(false);
            cont?.();
        }
    }),
    dogfood_save_some_files: Ymacs_Interactive(function(){
        this.ymacs.buffers
            .filter(b => b.dirty())
            .forEach(b => b.cmd("dogfood_save_file"));
    }),
    dogfood_magit_status: Ymacs_Interactive(async function(){
        this.cmd("dogfood_save_file", async () => {
            let result = await (await post(`ymacs-magit-status/${this.name}`)).json();
            console.log(result);
        });
    }),
});

Ymacs_Keymap.get("emacs").defineKeys({
    "C-x C-f" : "dogfood_load_file",
    "C-x C-s" : "dogfood_save_file",
    "C-x s"   : "dogfood_save_some_files",
    "C-z"     : "switch_to_buffer",
    "F1"      : "dogfood_magit_status",
});
