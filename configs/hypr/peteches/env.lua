local home = os.getenv("HOME") or "/home/peteches"
local guix_home_profile = home .. "/.guix-home/profile"
local guix_current_profile = home .. "/.config/guix/current"
local current_path = os.getenv("PATH") or ""
local current_xdg_data_dirs = os.getenv("XDG_DATA_DIRS") or ""

local function path_prepend(prefix, existing)
  if existing == nil or existing == "" then
    return prefix
  end
  return prefix .. ":" .. existing
end

-- DMS is launched by Hyprland rather than by an interactive login shell.  Make
-- the Guix Home profile visible to DMS/Quickshell so helper commands such as
-- matugen, dms, playerctl, brightnessctl, etc. resolve consistently.
hl.env("PATH", path_prepend(guix_home_profile .. "/bin", path_prepend(guix_current_profile .. "/bin", current_path)))
hl.env("XDG_DATA_DIRS", path_prepend(guix_home_profile .. "/share", path_prepend(guix_current_profile .. "/share", current_xdg_data_dirs)))

hl.env("QT_QPA_PLATFORMTHEME", "qt6ct")
hl.env("QT_QPA_PLATFORMTHEME_QT6", "qt6ct")
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
