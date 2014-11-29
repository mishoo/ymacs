DEFINE_SINGLETON("Ymacs_Keymap_Help", Ymacs_Keymap, function(D, P){
    P.defaultHandler = [ Ymacs_Interactive("^", function(){
        var ev = this.interactiveEvent();
	var key = Ymacs_Keymap.unparseKey (ev);
	if (! (this.currentHelpKeys instanceof Array)) {
	    this.currentHelpKeys = [];
	}
	var cc = this.currentHelpKeys;
	cc.push(key);
	var foundPrefix = false;
	var handled = false;

	var buf = ymacs.getActiveBuffer ();
	buf.keymap.r_foreach (function (km) {
	    var h = km.getHandler (cc);
	    if (h instanceof Array) {
		if (h[0] instanceof Function) {
		    cmd = func.ymacsCommand;
		} else {
		    cmd = h[0];
		}
		buf.setMinibuffer (cmd);

	    } else if (h) {
		foundPrefix = true;
	    }
	});

	if (foundPrefix)
	    return true;

	cc.splice (0, cc.length);

        this.popKeymap(Ymacs_Keymap_Help());
        return true;
    }) ];
});

Ymacs_Buffer.newCommands({
    help_c: Ymacs_Interactive (function () {
	this.pushKeymap (Ymacs_Keymap_Help ());
	if (! this.isMinibuffer)
	    this.setMinibuffer ("C-h c");
    }),

    help_m: Ymacs_Interactive (function () {
	var bname = "*Help*";
	var buf;
	if ((buf = ymacs.getBuffer (bname)) == null)
	    buf = ymacs.createBuffer ({name: bname});
	buf.cmd ("switch_to_buffer", bname);
	buf.setCode ("hello");
	var lines = [];
	this.keymap.r_foreach (function (km) {
	    var keys = km.constructor.KEYS; 
	    for (i in keys) {
		var val = keys[i];
		if (val instanceof Function)
		    val = val.toString ();
		lines.push (i + ": " + val);
	    }
	});
	buf.setCode (lines.join ("\n"));
    })
    
});

Ymacs_Keymap_Emacs().defineKeys({
    "C-h c": "help_c",
    "C-h m": "help_m"
});
