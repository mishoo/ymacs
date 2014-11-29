var dlg = new DlDialog({ title: "Ymacs", resizable: true });
var layout = new DlLayout({ parent: dlg });

var empty = new Ymacs_Buffer({ name: "empty" });
var ymacs = window.ymacs = new Ymacs({ buffers: [ empty ] });
ymacs.setColorTheme([ "dark", "y" ]);

layout.packWidget(ymacs, { pos: "bottom", fill: "*" });
dlg._focusedWidget = ymacs;
dlg.setSize({ x: 800, y: 600 });
dlg.show(true);
dlg.maximize(true);
