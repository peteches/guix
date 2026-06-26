Core = require("peteches.keybinds.core")
-- -----------------------------------------------------------------------------
-- Workspaces
--
-- Hyprland numeric workspace IDs start at 1. The physical 0 key is mapped to
-- workspace 10 rather than workspace 0.
-- -----------------------------------------------------------------------------

function bind_workspace(key, workspace)
  hl.bind(Core.mod .. " + " .. key, hl.dsp.focus({
    workspace = workspace,
    on_current_monitor = true,
  }), {
    description = "Focus workspace " .. workspace .. " on current monitor",
  })

  hl.bind(Core.mod .. " + SHIFT + " .. key, hl.dsp.window.move({
    workspace = workspace,
    follow = true,
  }), {
    description = "Move window to workspace " .. workspace .. " and follow",
  })
end

for i = 1, 9 do
  bind_workspace(tostring(i), i)
end
bind_workspace("0", 10)
