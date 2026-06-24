hl.env("QT_QPA_PLATFORMTHEME", "qt6ct")
hl.env("NVD_BACKEND", "direct+")	
hl.env("XDG_CURRENT_DESKTOP", "Hyprland")
hl.env("XDG_SESSION_TYPE", "wayland")
hl.env("XDG_SESSION_DESKTOP", "Hyprland")
-- Hyprcursor is used by Hyprland/Qt/Chromium/Electron paths; XCursor stays
-- set as a fallback for GTK and clients without server-side cursor support.
hl.env("HYPRCURSOR_THEME", "phinger-cursors-dark")
hl.env("HYPRCURSOR_SIZE", "32")
hl.env("XCURSOR_THEME", "phinger-cursors-dark")
hl.env("XCURSOR_SIZE", "32")
