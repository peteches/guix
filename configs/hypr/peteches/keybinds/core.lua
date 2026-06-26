Local = require("peteches.locals")
Lib = require("peteches.keybinds.lib")

M = {}

M.mod = "SUPER"
M.leader = M.mod .. " + Space"

-- -----------------------------------------------------------------------------
-- Direct high-frequency binds
-- -----------------------------------------------------------------------------

hl.bind(M.mod .. " + Return", hl.dsp.exec_cmd(Local.term_cmd), {
  description = "Open terminal",
})

hl.bind(M.mod .. " + e", hl.dsp.exec_cmd(Local.editor_cmd), {
  description = "Open editor",
})

hl.bind(M.mod .. " + b", hl.dsp.exec_cmd(Local.browser_cmd), {
  description = "Open browser",
})

hl.bind(M.mod .. " + d", hl.dsp.exec_cmd("wofi --show drun"), {
  description = "Open application launcher",
})

hl.bind(M.mod .. " + p", hl.dsp.exec_cmd(Local.password_lookup_cmd), {
  description = "Password lookup",
})

hl.bind("Print", hl.dsp.exec_cmd(Local.screenshot_cmd), {
  description = "Screenshot",
})

hl.bind(M.mod .. " + SHIFT + s", hl.dsp.exec_cmd(Local.screenshot_cmd), {
  description = "Screenshot",
})

hl.bind(M.mod .. " + SHIFT + d", hl.dsp.exec_cmd("dms ipc call notifications dismissAllPopups"), {
  description = "Dismiss notification popups",
})

hl.bind(M.mod .. " + SHIFT + w", hl.dsp.exec_cmd(Local.random_wallpaper_cmd), {
  description = "Choose random wallpaper",
})

hl.bind(M.mod .. " + Tab", hl.dsp.focus({ last = true }), {
  description = "Focus last window",
})

hl.bind(M.mod .. " + q", hl.dsp.window.close(), {
  description = "Close active window",
})

-- Universal escape hatch for any command-M.mode submap.
hl.bind(M.mod .. " + escape", hl.dsp.submap("reset"), {
  submap_universal = true,
  description = "Reset Hyprland keymap",
})

return M
