Core = require("peteches.keybinds.core")
-- -----------------------------------------------------------------------------
-- Emacs-style direct window navigation
-- -----------------------------------------------------------------------------

hl.bind(Core.mod .. " + CONTROL + b", hl.dsp.focus({ direction = "left" }), {
  description = "Focus window left",
})

hl.bind(Core.mod .. " + CONTROL + f", hl.dsp.focus({ direction = "right" }), {
  description = "Focus window right",
})

hl.bind(Core.mod .. " + CONTROL + p", hl.dsp.focus({ direction = "up" }), {
  description = "Focus window up",
})

hl.bind(Core.mod .. " + CONTROL + n", hl.dsp.focus({ direction = "down" }), {
  description = "Focus window down",
})

-- Mouse support is especially useful once floating windows enter the workflow.
hl.bind(Core.mod .. " + mouse:272", hl.dsp.window.drag(), {
  mouse = true,
  description = "Drag active window",
})
