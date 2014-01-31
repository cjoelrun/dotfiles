$fallthrough = "";
define_keymap = function x () { return null; };
define_key = define_keymap;
gerrit_keymap = define_keymap;

require("content-buffer.js");
define_keymap("gerrit_keymap", $display_name = "Gerrit");
define_key(gerrit_keymap, "/", null, $fallthrough);
define_key(gerrit_keymap, "?", null, $fallthrough);
define_key(gerrit_keymap, "o", null, $fallthrough);
define_key(gerrit_keymap, "j", null, $fallthrough);
define_key(gerrit_keymap, "k", null, $fallthrough);
define_key(gerrit_keymap, "n", null, $fallthrough);
define_key(gerrit_keymap, "p", null, $fallthrough);
define_key(gerrit_keymap, "g", null, $fallthrough);
define_key(gerrit_keymap, "a", null, $fallthrough);
define_key(gerrit_keymap, "m", null, $fallthrough);
define_key(gerrit_keymap, "return", null, $fallthrough);
define_key(gerrit_keymap, "u", null, $fallthrough);
define_key(gerrit_keymap, "O", null, $fallthrough);
define_key(gerrit_keymap, "N",n null, $fallthrough);
define_key(gerrit_keymap, "P", null, $fallthrough);
define_key(gerrit_keymap, "[", null, $fallthrough);
define_key(gerrit_keymap, "]", null, $fallthrough);
define_key(gerrit_keymap, "f", null, $fallthrough);
define_key(gerrit_keymap, "d", null, $fallthrough);
define_key(gerrit_keymap, "i", null, $fallthrough);
define_key(gerrit_keymap, "s", null, $fallthrough);
define_key(gerrit_keymap, "w", null, $fallthrough);
define_key(gerrit_keymap, "c", null, $fallthrough);
define_key(gerrit_keymap, "r", null, $fallthrough);
define_key(gerrit_keymap, "escape", null, $fallthrough);
define_key(gerrit_keymap, "C-s", null, $fallthrough);

var gerrit_modality = {
    normal: gerrit_keymap
};

define_page_mode("gerrit-mode",
		 build_url_regexp($domain ="review.openstack",
				  $tlds = ["org"]),

		 function enable (buffer) {
		     buffer.content_modalities.push(gerrit_modality);
		 },
		 function disable (buffer) {
		     var i = buffer.content_modalities.indexOf(gerrit_modality);
		     if (i > -1)
			 buffer.content_modalities.splice(i, 1);
		 },
		 $display_name = "Gerrit";);

page_mode_activate(gerrit-mode);
provide("gerrit");
