chrome.app.runtime.onLaunched.addListener(function() {
    chrome.app.window.create("chromeapp/emacs.html",
			     {
				 "bounds": {
				     "width": 640,
				     "height": 500
				 }
			     });
});
