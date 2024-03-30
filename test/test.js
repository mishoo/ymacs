import { Ymacs, Ymacs_Keymap } from "../src/index.js";

window.Ymacs_Keymap = Ymacs_Keymap;

window.addEventListener("beforeunload", ev => {
    ev.preventDefault();
    return ev.returnValue = true;
});

{
    let ymacs = window.ymacs = new Ymacs();
    Object.assign(ymacs.getElement().style, {
        position : "absolute",
        left     : 0,
        top      : 0,
        width    : "100%",
        height   : "100%"
    });
    ymacs.setColorTheme([ "material" ]);
    //ymacs.setColorTheme([ "base16-apathy" ]);
    //ymacs.setColorTheme([ "sanityinc-tomorrow-night" ]);
    //ymacs.setColorTheme([ "sanityinc-tomorrow-day" ]);
    //ymacs.setColorTheme([ "sanityinc-tomorrow-blue" ]);
    //ymacs.setColorTheme([ "sanityinc-tomorrow-bright" ]);
    //ymacs.setColorTheme([ "sanityinc-tomorrow-eighties" ]);
    let b = ymacs.getActiveBuffer.bind(ymacs);
    b().cmd("load_file", "test.html");

    // b().cmd("load_file", "ymacs-frame.js");
    // b().callInteractively("split_frame_horizontally");
    // b().cmd("other_frame");
    // b().cmd("load_file", "queen-board.lisp");
    // b().callInteractively("split_frame_vertically");
    // b().cmd("load_file", "README.md");

    document.body.appendChild(ymacs.getElement());
    ymacs.focus();

    b().cmd("toggle_line_numbers");
}
















(function(){                    // commented out.

/*

  Note that this file is just an example.  It should not be treated as
  part of Ymacs itself.  Ymacs is just an editing platform and as such
  it has no menus, no toolbar etc.  These can be easily added using
  other DynarchLIB widgets, as this file demonstrates.

  If a collection of useful menus/toolbars will emerge, a new compound
  widget will be defined.

*/

var desktop = new DlDesktop({});

function print(obj) {
        var a = [], i;
        for (i in obj) {
                var val = obj[i];
                if (val instanceof Function)
                        val = val.toString();
                else
                        val = JSON.stringify(val);
                a.push(JSON.stringify(i) + " : " + val);
        }
        return a.map(function(line){
                return line.replace(/^/mg, function(s) {
                        return "        ";
                });
        }).join("\n");
};

var info = ( "Existing keybindings:\n\n" +
             print(Ymacs_Keymap.get("emacs").constructor.KEYS) // XXX no constructor.KEYS now.
             + "\n\nHave fun!\n" );

var lorem = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam sagittis posuere dui, id facilisis metus blandit nec. Ut pulvinar felis vitae lacus mattis fermentum semper risus aliquet. Sed nec dolor quis odio condimentum pellentesque. Donec non vehicula massa. Nulla a rutrum nulla. Morbi dapibus pharetra ligula, ac pharetra purus scelerisque sit amet. Nulla non velit ut urna gravida rutrum non vitae leo. Duis gravida, lacus eget laoreet semper, magna sem scelerisque dolor, a sagittis lacus justo nec lectus. Vivamus lacus massa, mattis ut rutrum ac, consectetur vel ipsum. Suspendisse potenti. Fusce convallis lorem vel dui tristique non viverra mi feugiat. Vivamus mollis rutrum porta. Nunc non purus ut sapien pretium tristique aliquam sit amet eros. Vivamus vel rutrum lacus.\n\
\n\
Nullam vitae tellus enim, id suscipit nisl. Mauris elementum scelerisque lacus ac pellentesque. Donec rutrum tellus vel leo lacinia semper. Nulla porta, elit non vulputate pulvinar, eros lacus euismod libero, ut laoreet erat lacus a est. Nam quis mi nec nisl aliquam tempor eget vel massa. Sed justo ante, ornare ut tristique a, laoreet ac justo. Proin gravida cursus mauris a porttitor. Aliquam elit justo, euismod suscipit pharetra ut, placerat et dolor. Donec pulvinar elit nec ligula gravida scelerisque. Aenean rutrum tempus dui at volutpat. Maecenas a justo quis libero vehicula fermentum sit amet in augue. In ante nulla, fermentum at rutrum id, tincidunt ut massa. Vivamus quis justo ut quam tempor ultricies ultricies vitae tellus. Pellentesque lorem elit, convallis ut congue at, porta non nisi. Curabitur lectus tortor, elementum venenatis faucibus ut, vulputate vehicula dui. Fusce in dui id est lobortis venenatis eu ut dolor. Quisque vel diam diam. Nulla porttitor adipiscing nisi eget cursus. .\n\n".repeat(10);

        var javascript = new Ymacs_Buffer({ name: "test.js" });

        javascript.setCode("\
/* Note that there are a few buffers already loaded.\n\
   You can switch through them using C-TAB or C-S-TAB.\n\
   You can also split frames using C-x 2 or C-x 3, or\n\
   revert to a single frame (the active one) with C-x 1.\n\
 */\n\
\n\
function () {\n\
        alert(\"moo\");\n\
        while (/[/]/.test(str)) {\n\
    // an unescaped slash can appear in a character set in regexps\n\
        }\n\
\n\
/**\n\
* press TAB on these lines to fix indentation,\n\
* or move the caret to the first “{” character and press C-M-q\n\
*/\n\
        return function(){\n\
      alert(this.foo);\n\
      this.addClass(this._horiz ? 'Ymacs_SplitCont_horiz' : 'Ymacs_SplitCont_vert');\n\
}.bind(this);\n\
}\n\
\n\
// select the following few lines and try M-x eval_region\n\
// then try \"C-x w\" to count the words in this buffer\n\
//\n\
// Note that once defined, it's not easy to change a keymap...\n\
// I should figure out how to fix this.\n\
DEFINE_SINGLETON(\"My_Keymap\", Ymacs_Keymap, function(D){\n\
    D.KEYS = {\n\
        \"C-x w\": function() {\n\
            this.cmd(\"save_excursion\", function(){\n\
                this.cmd(\"beginning_of_buffer\");\n\
                var count = 0;\n\
                while (!this.cmd(\"eob_p\")) {\n\
                    this.cmd(\"forward_word\");\n\
                    ++count;\n\
                }\n\
                alert(count + \" words in this buffer\");\n\
            });\n\
        }\n\
    };\n\
});\n\
this.pushKeymap(My_Keymap());\n\
");

        var xml = new Ymacs_Buffer({ name: "index.html" });
        xml.setCode("\
<html>\n\
  <head>\n\
    <title>Ymacs -- Open Source Source Code Editor. :-p</title>\n\
  </head>\n\
  <body style=\"margin: 1em auto; width: 80%\"\n\
        id=\"foo\">\n\
    <h1 class=\"PageTitle\">Cool, isn't it?</h1>\n\
  </body>\n\
</html>\
");

        var txt = new Ymacs_Buffer({ name: "lorem.txt" });
        txt.setCode(lorem);

        javascript.cmd("javascript_mode");
        javascript.setq("indent_level", 4);
        xml.cmd("xml_mode");

        var lisp = new Ymacs_Buffer({ name: "test.lisp" });
        lisp.setCode(";; Some basic Common-Lisp highlighting and indentation\n\
;; Parens are auto-inserted\n\
;; And you can close all remaining parens with C-c ] or C-c C-]\n\
\n\
(defun foo ())\n");
        lisp.cmd("lisp_mode");

        var markdown = new Ymacs_Buffer({ name: "markdown.txt" });
        markdown.setCode("\
# Markdown sytnax\n\
\n\
For now there is only basic syntax highlighting, but note\n\
that paragraph filling commands (M-q, M-S-q) are good enough\n\
to make editing Markdown a snap.  For example, add a > character\n\
at the start of this paragraph, then press M-q.  It will \"quote\"\n\
all the text in the paragraph.  Press M-S-q to clear the prefix.\n\
\n\
Type \"1. \" at the start of the paragraph, then press M-q and it\n\
will format a list item.  If you press M-ENTER within it it will start\n\
the next list item.  Generally, M-ENTER starts a paragraph \"similar\"\n\
to the current one.\n\
");
        markdown.cmd("markdown_mode");

        var keys = new Ymacs_Buffer({ name: "keybindings.txt" });
        keys.setCode(info);

        var layout = new DlLayout({ parent: desktop });

        var empty = new Ymacs_Buffer({ name: "empty" });
        var ymacs = window.ymacs = new Ymacs({
                buffers: [ javascript, xml, lisp, markdown, txt, keys ],
                className: "Ymacs-blinking-caret"
        });
        ymacs.setColorTheme([ "dark", "material-dark" ]);

        try {
                ymacs.getActiveBuffer().cmd("eval_file", ".ymacs");
        } catch(ex) {}

        var menu = new DlHMenu({});
        menu.setStyle({ marginLeft: 0, marginRight: 0 });

        var item = new DlMenuItem({ parent: menu, label: "Load its own code!".makeLabel() });

        var files = [
                "ymacs.js",
                "ymacs-frame.js",
                "ymacs-textprop.js",
                "ymacs-exception.js",
                "ymacs-interactive.js",
                "ymacs-buffer.js",
                "ymacs-marker.js",
                "ymacs-commands.js",
                "ymacs-commands-utils.js",
                "ymacs-keymap.js",
                "ymacs-keymap-emacs.js",
                "ymacs-keymap-isearch.js",
                "ymacs-minibuffer.js",
                "ymacs-tokenizer.js",
                "ymacs-mode-paren-match.js",
                "ymacs-mode-lisp.js",
                "ymacs-mode-js.js",
                "ymacs-mode-xml.js",
                "ymacs-mode-css.js",
                "ymacs-mode-markdown.js"
        ];
        var submenu = new DlVMenu({});
        item.setMenu(submenu);
        files.forEach(function(file){
                var item = new DlMenuItem({ label: file, parent: submenu });
                item.addEventListener("onSelect", function(){
                        var request = new DlRPC({ url: YMACS_SRC_PATH + file + "?killCache=" + new Date().getTime() });
                        request.call({
                                callback: function(data){
                                        var code = data.text;
                                        var buf = ymacs.getBuffer(file) || ymacs.createBuffer({ name: file });
                                        buf.setCode(code);
                                        buf.cmd("javascript_mode");
                                        ymacs.switchToBuffer(buf);
                                }
                        });
                });
        });

        var item = new DlMenuItem({ parent: menu, label: "Set indentation level".makeLabel() });
        item.addEventListener("onSelect", function() {
                var buf = ymacs.getActiveBuffer(), newIndent;
                newIndent = prompt("Indentation level for the current buffer: ", buf.getq("indent_level"));
                if (newIndent != null)
                        newIndent = parseInt(newIndent, 10);
                if (newIndent != null && !isNaN(newIndent)) {
                        buf.setq("indent_level", newIndent);
                        buf.signalInfo("Done setting indentation level to " + newIndent);
                }
        });

        menu.addFiller();

        var item = new DlMenuItem({ parent: menu, label: "Toggle line numbers".makeLabel() });
        item.addEventListener("onSelect", function() {
                ymacs.getActiveBuffer().cmd("toggle_line_numbers");
        });

        /* -----[ color theme ]----- */

        var item = new DlMenuItem({ parent: menu, label: "Color theme".makeLabel() });
        var submenu = new DlVMenu({});
        item.setMenu(submenu);

        [
                "dark|y|Dark background (default)",
                "dark|material-dark|>Material-dark",
                "dark|mishoo|>Mishoo's Emacs theme",
                "dark|billw|>Billw",
                "dark|charcoal-black|>Charcoal black",
                "dark|clarity-and-beauty|>Clarity and beauty",
                "dark|classic|>Classic",
                "dark|gnome2|>Gnome 2",
                "dark|calm-forest|>Calm forest",
                "dark|linh-dang-dark|>Linh Dang Dark",
                "dark|blue-mood|>Blue mood",
                "dark|zenburn|>Zenburn",
                "dark|standard-dark|>Emacs standard (dark)",
                null,
                "light|y|Light background (default)",
                "light|andreas|>Andreas",
                "light|bharadwaj|>Bharadwaj",
                "light|gtk-ide|>GTK IDE",
                "light|high-contrast|>High contrast",
                "light|scintilla|>Scintilla",
                "light|standard-xemacs|>Standard XEmacs",
                "light|vim-colors|>Vim colors",
                "light|standard|>Emacs standard (light)"
        ].forEach(function(theme){
                if (theme == null) {
                        submenu.addSeparator();
                } else {
                        theme = theme.split(/\s*\|\s*/);
                        var label = theme.pop();
                        label = label.replace(/^>\s*/, "&nbsp;".repeat(4));
                        var item = new DlMenuItem({ parent: submenu, label: label });
                        item.addEventListener("onSelect", ymacs.setColorTheme.bind(ymacs, theme));
                }
        });

        function setFrameStyle(style) {
                ymacs.setFrameStyle(style);
        }

        /* -----[ font ]----- */

        var item = new DlMenuItem({ parent: menu, label: "Font family".makeLabel() });
        var submenu = new DlVMenu({});
        item.setMenu(submenu);

        item = new DlMenuItem({ parent: submenu, label: "Default from ymacs.css" });
        item.addEventListener("onSelect", function(){
                setFrameStyle({ fontFamily: "" });
        });

        submenu.addSeparator();

        [
                "Lucida Sans Typewriter",
                "Andale Mono",
                "Courier New",
                "Arial",
                "Verdana",
                "Tahoma",
                "Georgia",
                "Times New Roman"

        ].forEach(function(font){
                item = new DlMenuItem({ parent: submenu, label: "<span style='font-family:" + font + "'>" + font + "</span>" });
                item.addEventListener("onSelect", function(){
                        setFrameStyle({ fontFamily: font });
                });
        });

        // ymacs.getActiveFrame().setStyle({ fontFamily: "Arial", fontSize: "18px" });

        /* -----[ font size ]----- */

        var item = new DlMenuItem({ parent: menu, label: "Font size".makeLabel() });
        var submenu = new DlVMenu({});
        item.setMenu(submenu);

        item = new DlMenuItem({ parent: submenu, label: "Default from ymacs.css" });
        item.addEventListener("onSelect", function(){
                setFrameStyle({ fontSize: "" });
        });

        submenu.addSeparator();

        [
                "11px",
                "12px",
                "14px",
                "16px",
                "18px",
                "20px",
                "22px",
                "24px"

        ].forEach(function(font){
                item = new DlMenuItem({ parent: submenu, label: "<span style='font-size:" + font + "'>" + font + "</span>" });
                item.addEventListener("onSelect", function(){
                        setFrameStyle({ fontSize: font });
                });
        });

        layout.packWidget(menu, { pos: "top" });
        layout.packWidget(ymacs, { pos: "bottom", fill: "*" });

        desktop.fullScreen();
        desktop.callHooks("onResize");

DynarchDomUtils.trash(document.getElementById("x-loading"));

ymacs.getActiveBuffer().cmd("load_file", "ymacs-frame.js");

});
