DEFINE_SINGLETON("Ymacs_Keymap_Dired", Ymacs_Keymap, function(D, P){

    D.KEYS = {
        "e"       : "dired_edit_file",
    };

    P.defaultHandler = [ "keypress_readonly" ];
});

Ymacs_Buffer.newMode("dired_mode", function(){

    var keymap = Ymacs_Keymap_Dired();
    this.pushKeymap(keymap);
    return function() {
        this.popKeymap(keymap);
    };

});

Ymacs_Buffer.newCommands({

    dired_edit_file: Ymacs_Interactive(function() {
        var position = ymacs.getActiveBuffer()._positionToRowCol(ymacs.getActiveBuffer().point());
        var entry = ymacs.getActiveBuffer().chromeapp_directory_list[position.row];
        if (entry.isDirectory) {
            cfs_open_directory(entry);
        } else {
            cfs_open_file(entry);
        }
    }),

    keypress_readonly: Ymacs_Interactive(function() {
        var ev = this.interactiveEvent();
        if (ev.charCode && !ev.ctrlKey && !ev.altKey) {
            return true;
        } else if (ev.keyCode != 0 || ev.ctrlKey || ev.altKey) {
            return false;
        }
    })
});
