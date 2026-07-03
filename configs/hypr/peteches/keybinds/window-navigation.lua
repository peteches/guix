Core = require("peteches.keybinds.core")
-- -----------------------------------------------------------------------------
-- Emacs-style direct window navigation
-- -----------------------------------------------------------------------------
local binds = {
    [Core.mod .. " + CONTROL + b"] = "left",
    [Core.mod .. " + CONTROL + f"] = "right",
    [Core.mod .. " + CONTROL + p"] = "up",
    [Core.mod .. " + CONTROL + n"] = "down",
    [Core.mod .. " + left"] = "left",
    [Core.mod .. " + right"] = "right",
    [Core.mod .. " + up"] = "up",
    [Core.mod .. " + down"] = "down",
}

for bind, direction in pairs(binds) do
    hl.bind(bind, hl.dsp.focus({ direction = direction }), {
	description = "Focus window " .. direction
    })
end

-- Mouse support is especially useful once floating windows enter the workflow.
hl.bind(Core.mod .. " + mouse:272", hl.dsp.window.drag(), {
  mouse = true,
  description = "Drag active window",
})
