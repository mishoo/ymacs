import { Ymacs, Ymacs_Keymap, Ymacs_Interactive, Ymacs_Buffer, Ymacs_Tokenizer } from "../src/index.js";

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
                  case /web-mode|html/.test(mode):
                    buf.cmd(/\.syt/.test(name) ? "sytes_mode" : "html_mode");
                    break;
                  case /xml/.test(mode):
                    buf.cmd("xml_mode");
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



(function(){

    // Sytes mode - this is mostly useful to me, so I didn't bother
    // cleaning it up yet.. I'll just let it stay here.
    //
    // https://lisperator.net/sytes/

    Ymacs_Tokenizer.define("sytes", function(stream, tok){

        var PARSER = {
            next: next,
            copy: copy,
            indentation: indentation,
            get passedParens() {
                return [
                    ...$passedParens,
                    ...$lisp.passedParens,
                    ...$html.passedParens,
                ];
            },
            get tag() {
                return $html.tag;
            },
        };

        var $passedParens = [];
        var $parens = [];
        var $lisp_options = {};
        var $syntax_start, $syntax_stop;
        var $syntax_re_start, $syntax_re_stop;

        setSyntax("{", "}");

        var $lisp = tok.getLanguage("lisp", $lisp_options);
        var $html = tok.getLanguage("html");
        var $mode = $html;

        function setSyntax(a, b) {
            $syntax_start = a;
            $syntax_stop = b;
            $syntax_re_start = new RegExp("^\\" + a);
            $syntax_re_stop = new RegExp("^\\" + b);
            $lisp_options.rx_special = new RegExp("[\\" + a + "\\" + b + "]");
        };

        function foundToken(c1, c2, type) {
            tok.onToken(stream.line, c1, c2, type);
        };

        function switchToLisp() {
            $mode = $lisp;
            $lisp._formLen = 0;
        };

        function switchToHtml() {
            $mode = $html;
        };

        function next() {
            var m;
            stream.checkStop();
            if ($mode === $html) {
                if (stream.lookingAt("\\" + $syntax_start)) {
                    foundToken(stream.col, ++stream.col, "comment");
                    foundToken(stream.col, ++stream.col);
                    return true;
                }
                if (stream.lookingAt(/^\\;/) && stream.col == 0) {
                    foundToken(stream.col, ++stream.col, "comment");
                    foundToken(stream.col, ++stream.col);
                    return true;
                }
                if ((m = stream.lookingAt(/^(;+)(.*)/)) && stream.col == 0) {
                    foundToken(stream.col, stream.col += m[1].length, "comment-starter");
                    foundToken(stream.col, stream.col += m[2].length, "comment");
                    return true;
                }
                if ((m = stream.lookingAt($syntax_re_stop))) {
                    let p = $parens.pop();
                    if (p && p.new_syntax) {
                        p.closed = { line: stream.line, col: stream.col, opened: p };
                        $passedParens.push(p);
                        setSyntax(p.new_syntax[0], p.new_syntax[1]);
                        foundToken(stream.col, stream.col += m[0].length, "regexp-stopper");
                    }
                    else {
                        switchToLisp();
                        $lisp.popInParen($syntax_start, $syntax_stop.length, "regexp-stopper");
                    }
                    return true;
                }
                if (stream.lookingAt($syntax_re_start)) {
                    stream.col++;
                    if (stream.lookingAt(".SYNTAX")) {
                        let p = { line: stream.line, col: stream.col - 1, type: $syntax_start };
                        $parens.push(p);
                        foundToken(stream.col - 1, stream.col, "regexp-starter");
                        foundToken(stream.col, stream.col += 7, "builtin");
                        if ((m = stream.lookingAt(/^(\s*)(".")(\s+)(".")/i))) {
                            foundToken(stream.col += m[1].length, stream.col += m[2].length, "string");
                            foundToken(stream.col += m[3].length, stream.col += m[4].length, "string");
                            p.new_syntax = [ m[2].charAt(1), m[4].charAt(1) ];
                        }
                        else {
                            foundToken(stream.col, ++stream.col);
                        }
                    }
                    else {
                        stream.col--;
                        switchToLisp();
                        $lisp.pushInParen($syntax_start, "regexp-starter");
                    }
                    return true;
                }
                return $html.next();
            }
            if ($mode === $lisp) {
                if (stream.peek() == $syntax_stop) {
                    $lisp.popInParen($syntax_start, $syntax_stop.length, "regexp-stopper");
                    switchToHtml();
                    return true;
                }
                else if (stream.peek() == $syntax_start) {
                    $lisp.pushInParen($syntax_start, "regexp-starter");
                    switchToHtml();
                    return true;
                }
                return $lisp.next();
            }
        };

        function copy() {
            var _lisp = $lisp.copy();
            var _html = $html.copy();
            var _mode = $mode;
            var _passedParens = $passedParens.slice();
            var _parens = $parens.slice();
            var _syntax_start = $syntax_start;
            var _syntax_stop = $syntax_stop;
            var _syntax_re_start = $syntax_re_start;
            var _syntax_re_stop = $syntax_re_stop;
            function resume() {
                $lisp = _lisp();
                $html = _html();
                $mode = _mode;
                $passedParens = _passedParens.slice();
                $parens = _parens.slice();
                $syntax_start = _syntax_start;
                $syntax_stop = _syntax_stop;
                $syntax_re_start = _syntax_re_start;
                $syntax_re_stop = _syntax_re_stop;
                return PARSER;
            };
            return resume;
        };

        function indentation() {
            return $mode.indentation();
        };

        return PARSER;

    });

    function define_mode(name, tok) {
        Ymacs_Buffer.newMode(name, function(){
            var old_tok = this.tokenizer;
            var was_html_mode = this.cmd("html_mode", true);
            var was_paren_match = this.cmd("paren_match_mode", true);
            var changed_vars = this.setq({ indent_level: 2 });
            this.setq("fill_column", 120);
            this.setTokenizer(new Ymacs_Tokenizer({ type: tok, buffer: this }));
            return function() {
                this.setq(changed_vars);
                if (!was_html_mode)
                    this.cmd("html_mode", false);
                if (!was_paren_match)
                    this.cmd("paren_match_mode", false);
                this.setTokenizer(old_tok);
            };
        });
    };

    define_mode("sytes_mode", "sytes");

})();
