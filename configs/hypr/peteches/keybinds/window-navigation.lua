Core = require("peteches.keybinds.core")
Emacs = require("peteches.keybinds.emacs")

-- -----------------------------------------------------------------------------
-- Emacs-style direct window navigation
-- -----------------------------------------------------------------------------
local binds = {b}

for k,v in pairs(Emacs.movement) do 
    binds[Core.mod .. " + CONTROL + " .. k] = v
end


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
