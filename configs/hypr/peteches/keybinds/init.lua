-- Hyprland keybinds: command-mode oriented v1
--
-- Design goals:
-- - Keep direct binds only for high-frequency actions.
-- - Keep Emacs-style navigation: b/f/p/n = left/right/up/down.
-- - Put lower-frequency WM operations behind SUPER + Space submaps.
-- - Make window-to-workspace actions follow the moved window.
-- - Stay portable across laptop, desktop, and multi-monitor systems.

require("peteches.keybinds.core")
require("peteches.keybinds.window-navigation")
require("peteches.keybinds.workspace-navigation")
require("peteches.keybinds.multimedia")
require("peteches.keybinds.command-mode")
