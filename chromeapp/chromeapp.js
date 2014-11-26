function cfs_getFileContents(name, nothrow, cont) {
    chrome.fileSystem.chooseEntry({type: 'openFile'}, function(readOnlyEntry) {

	readOnlyEntry.file(function(file) {
	    var reader = new FileReader();

	    reader.onerror = function(e) {
		throw e;
	    }
	    reader.onloadend = function(e) {
		cont(e.target.result, null); // second parameter is file stamp, on a real fs it should be last modification time
	    };

	    reader.readAsText(file);
	});
    });
}

function cfs_setFileContents(name, content) {
    chrome.fileSystem.chooseEntry({type: 'saveFile'}, function(writableFileEntry) {
	writableFileEntry.createWriter(function(writer) {
	    writer.onerror = function(e) {
		throw e;
	    }
	    writer.onwriteend = function(e) {
                ymacs.getActiveBuffer().signalInfo("Saved.");
	    };
	    writer.write(new Blob([content], {type: 'text/plain'}));
	}, function(e) {
	    throw e;
	});
    });
}

ymacs.fs_getFileContents = function(name, nothrow, cont) {
    cfs_getFileContents(name, nothrow, cont);
};

ymacs.fs_setFileContents = function(name, content, stamp, cont) {
    cfs_setFileContents(name, content);
    cont(null); // Use stamp when we have one
};

function cfs_open_file(file_entry) {
    file_entry.file(function(file) {
        var reader = new FileReader();

        reader.onerror = function(e) {
            throw e;
        }
        reader.onloadend = function(contents) {
            var buffer = ymacs.getBuffer(file_entry.name);
            if (!buffer) {
                buffer = ymacs.createBuffer({name: file_entry.name, stamp: null});
            }
            buffer.setCode(contents.target.result || "");
            buffer.stamp = null;
            buffer.dirty(false);
            buffer.cmd("set_buffer_mode");
            buffer.cmd("switch_to_buffer", file_entry.name);
            buffer.chromeapp_entry = file_entry;
            chrome.fileSystem.getDisplayPath(file_entry, function (path) {
                buffer.chromeapp_filename = path;
            });
        };
	    
        reader.readAsText(file);
    });
}

Ymacs_Buffer.COMMANDS["find_file"] = Ymacs_Interactive("iChoose file from system UI", function () {
    chrome.fileSystem.chooseEntry({type: 'openFile'}, cfs_open_file);
});

Ymacs_Buffer.COMMANDS["write_file"] = Ymacs_Interactive("iChoose file to write to from system UI", function () {
    chrome.fileSystem.chooseEntry({type: 'saveFile',
                                   suggestedName: ymacs.getActiveBuffer().name},
                                  function(open_file) {
                                      open_file.createWriter(function(writer) {
                                          writer.onerror = function(e) {
                                              throw e;
                                          }
                                          writer.onwriteend = function() {
                                              writer.onwriteend = function () {
                                                  buffer = ymacs.getActiveBuffer()
                                                  buffer.name = open_file.name;
                                                  buffer.dirty(false);
                                                  buffer.chromeapp_entry = open_file;
                                                  chrome.fileSystem.getDisplayPath(open_file, function (path) {
                                                      buffer.chromeapp_filename = path;
                                                      buffer.signalInfo("Wrote " + path);
                                                  });
                                              };
                                              writer.write(new Blob([ymacs.getActiveBuffer().getCode()]), {type: 'text/plain'});
                                          };
                                          writer.truncate(0);
                                      }, function(e) {
                                          throw e;
                                      });
                                  });
});

function cfs_save_buffer(buffer) {
    var entry = buffer.chromeapp_entry;
    if (entry) {
        entry.createWriter(function(writer) {
            writer.onerror = function(e) {
                throw e;
            }
            writer.onwriteend = function(_) {
                writer.onwriteend = function (e) {
                    buffer.name = buffer.chromeapp_entry.name;
                    buffer.dirty(false);
                    buffer.signalInfo("Wrote " + buffer.chromeapp_filename);
                };
                writer.write(new Blob([buffer.getCode()]), {type: 'text/plain'});
            };
            writer.truncate(0);
        }, function(e) {
            throw e;
        });
    } else {
        buffer.cmd("write_file");
    }
}

Ymacs_Buffer.COMMANDS["save_buffer"] = Ymacs_Interactive("", function () {
    cfs_save_buffer(ymacs.getActiveBuffer());
});

function cfs_open_directory(directory_entry) {
    var buffer = ymacs.getBuffer(directory_entry.name);
    if (!buffer) {
        buffer = ymacs.createBuffer({name: directory_entry.name, stamp: null});
    }
    buffer.chromeapp_directory_entry = directory_entry;
    buffer.setCode("");
    directory_entry.createReader().readEntries(function (entries) {
        entries.sort(function (entry1, entry2) {
            return entry1.name > entry2.name;
        });
        buffer.chromeapp_directory_list = entries;
        var lines = [];
        for (e in entries) {
            var line;
            if (entries[e].isDirectory) {
                line = "d ";
            } else {
                line = "- ";
            }
            line += entries[e].name + "\n";
            lines.push(line);
        }
        buffer.setCode(lines.join(""));
        buffer.stamp = null;
        buffer.dirty(false);
        buffer.cmd("set_buffer_mode");
        buffer.cmd("dired_mode");
        buffer.cmd("switch_to_buffer", buffer.name);
    });
}

Ymacs_Buffer.COMMANDS["list_directory"] = Ymacs_Interactive("iChoose directory from system UI", function () {
    chrome.fileSystem.chooseEntry({type: 'openDirectory'}, cfs_open_directory);
});

function save_buffers_safe_exit() {
    if (ymacs.unsaved_buffers.length == 0) {
        if (ymacs.modified_buffers_exist) {
            ymacs.getActiveBuffer().cmd("minibuffer_yn", "Modified buffers exist; exit anyway?",
                       function (yes) {
                           if (yes) {
                               window.close();
                           }
                       });
        } else {
            window.close();
        }
        return;
    }
    var buffer = ymacs.unsaved_buffers.pop();
    var msg = "Save file " + buffer.chromeapp_filename + "?";
    buffer.cmd("minibuffer_yn", msg, function (yes) {
        if (yes) {
            cfs_save_buffer(buffer);
        } else {
            ymacs.modified_buffers_exist = true;
        }
        save_buffers_safe_exit();
    });
}

Ymacs_Buffer.newCommands({

    safe_exit: Ymacs_Interactive(function() {
        ymacs.modified_buffers_exist = false;
        ymacs.unsaved_buffers = [];
        for (b in ymacs.buffers) {
            if (isNaN(parseInt(b))) {
                continue;
            }
            var buffer = ymacs.buffers[b];
            if (buffer.dirty() && buffer.chromeapp_entry) {
                ymacs.unsaved_buffers.push(buffer);
            }
        }

        save_buffers_safe_exit();
    })
});

Ymacs_Keymap_Emacs().defineKeys({
    "C-x C-d": "list_directory",
    "C-x C-c": "safe_exit"
});
