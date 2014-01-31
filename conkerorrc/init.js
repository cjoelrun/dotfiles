// Style

url_completion_use_history = true;
url_remoting_fn = load_url_in_new_buffer;
mode_line_mode(false);
require("new-tabs.js"); // (Uses CSS (see chrome))

// Download dir
cwd=get_home_directory();
cwd.append("Downloads");

// Default Webjump
read_url_handler_list = [read_url_make_default_webjump_handler("google")];

// No new window for downloads
download_buffer_automatic_open_target=OPEN_NEW_BUFFER;

// Don't open download buffer automatically
//remove_hook("download_added_hook", open_download_buffer_automatically);

// Reload this file on the fly
interactive("rc-reload",
            "Reload the Conkerorrc.",
            function(I) { load_rc_file("~/.conkerorrc"); });

// Open external links in new buffer (for Emacs)
url_remoting_fn = load_url_in_new_buffer;

// Use emacsclient as external editor
editor_shell_command = "emacsclient -c"

// Clear history after 1 day
session_pref('browser.history_expire_days', 1);

// Reduce memory consumption
session_pref('browser.cache.memory.capacity', 4096);
session_pref('browser.cache.memory.enable', false);

// Reduce amount of time Firefox stores uncompressed images in memory
session_pref('mage.mem.min_discard_timeout_ms', 100000);

// Clear downloads history on completion
session_pref('browser.download.manager.retention', 0);

//Unfocus input fields
define_key(content_buffer_normal_keymap, "M-q", "unfocus");

//Follow new link in background buffer
define_key(content_buffer_normal_keymap, "d", "follow-new-buffer-background");

define_webjump("y", "http://www.youtube.com/results?search_query=%s&search=Search"); //Youtube
define_webjump("d", "http://duckduckgo.com/?q=%s"); //DuckDuckGo
define_webjump("github", "http://github.com/search?q=%s&type=Everything");
define_webjump("amazon", "http://www.amazon.com/exec/obidos/external-search/?field-keywords=%s&mode=blended");

xkcd_add_title = true;

// Google Bookmark
define_webjump("gbookmark", "javascript:(function(){var%20a=window,b=document,c=encodeURIComponent,d=a.open(\"http://www.google.com/bookmarks/mark?op=edit&output=popup&bkmk=\"+c(b.location)+\"&title=\"+c(b.title),\"bkmk_popup\",\"left=\"+((a.screenX||a.screenLeft)+10)+\",top=\"+((a.screenY||a.screenTop)+10)+\",height=420px,width=550px,resizable=1,alwaysRaised=1\");a.setTimeout(function(){d.focus()},300)})();")
