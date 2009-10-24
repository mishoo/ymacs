var desktop = new DlDesktop({});
desktop.fullScreen();

function print(obj) {
        var a = [], i;
        for (i in obj) {
                var val = obj[i];
                if (val instanceof Function)
                        val = val.toString();
                else
                        val = DlJSON.encode(val);
                a.push(DlJSON.encode(i) + " : " + val);
        }
        return a.map(function(line){
                return line.replace(/^/mg, function(s) {
                        return "        ";
                });
        }).join("\n");
};

var info = ( "Existing keybindings:\n\n" +
             print(Ymacs_Keymap_Emacs.KEYS)
             + "\n\nHave fun!\n" );

var lorem = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam sagittis posuere dui, id facilisis metus blandit nec. Ut pulvinar felis vitae lacus mattis fermentum semper risus aliquet. Sed nec dolor quis odio condimentum pellentesque. Donec non vehicula massa. Nulla a rutrum nulla. Morbi dapibus pharetra ligula, ac pharetra purus scelerisque sit amet. Nulla non velit ut urna gravida rutrum non vitae leo. Duis gravida, lacus eget laoreet semper, magna sem scelerisque dolor, a sagittis lacus justo nec lectus. Vivamus lacus massa, mattis ut rutrum ac, consectetur vel ipsum. Suspendisse potenti. Fusce convallis lorem vel dui tristique non viverra mi feugiat. Vivamus mollis rutrum porta. Nunc non purus ut sapien pretium tristique aliquam sit amet eros. Vivamus vel rutrum lacus.\n\
\n\
Nullam vitae tellus enim, id suscipit nisl. Mauris elementum scelerisque lacus ac pellentesque. Donec rutrum tellus vel leo lacinia semper. Nulla porta, elit non vulputate pulvinar, eros lacus euismod libero, ut laoreet erat lacus a est. Nam quis mi nec nisl aliquam tempor eget vel massa. Sed justo ante, ornare ut tristique a, laoreet ac justo. Proin gravida cursus mauris a porttitor. Aliquam elit justo, euismod suscipit pharetra ut, placerat et dolor. Donec pulvinar elit nec ligula gravida scelerisque. Aenean rutrum tempus dui at volutpat. Maecenas a justo quis libero vehicula fermentum sit amet in augue. In ante nulla, fermentum at rutrum id, tincidunt ut massa. Vivamus quis justo ut quam tempor ultricies ultricies vitae tellus. Pellentesque lorem elit, convallis ut congue at, porta non nisi. Curabitur lectus tortor, elementum venenatis faucibus ut, vulputate vehicula dui. Fusce in dui id est lobortis venenatis eu ut dolor. Quisque vel diam diam. Nulla porttitor adipiscing nisi eget cursus. .\n\n".x(10);

try {
        var dlg = new DlDialog({ title: "Ymacs", resizable: true, quitBtn: "destroy" });
        var layout = new DlLayout({ parent: dlg, outerSpace: 2 });
        var editor = window.editor = new Ymacs_Buffer({
                // code: info
                // code: "(defun f() (bar))\n(lambda (a b c) (list a b c))\n"
                code: lorem
        });
        var editor_frame = new Ymacs_Frame({ buffer: editor });
        var editor_frame2 = new Ymacs_Frame({ buffer: editor });

        var minibuffer = new Ymacs_Buffer({ highlightCurrentLine: false });
        var minibuffer_frame = new Ymacs_Frame({ buffer: minibuffer });
        editor.minibuffer = minibuffer;

        layout.packWidget(minibuffer_frame, { pos: "bottom" });
        layout.packWidget(new DlResizeBar({ horiz: true, widget: minibuffer_frame, invert: true }), { pos: "bottom" });
        layout.packWidget(editor_frame, { pos: "left", fill: "50%" });
        layout.packWidget(new DlResizeBar({ widget: editor_frame }), { pos: "left" });
        layout.packWidget(editor_frame2, { pos: "right", fill: "*" });
        dlg.setSize({ x: 800, y: 600 });
        dlg._focusedWidget = editor_frame;
        dlg.show(true);
        dlg.maximize(true);
} catch(ex) {
        console.log(ex);
}

DynarchDomUtils.trash($("x-loading"));
