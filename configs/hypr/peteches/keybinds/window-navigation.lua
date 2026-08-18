Core = require("peteches.keybinds.core")
Emacs = require("peteches.keybinds.emacs")

-- -----------------------------------------------------------------------------
-- Emacs-style direct window navigation
-- -----------------------------------------------------------------------------
local binds = {b}

for k,v in pairs(Emacs.movement) do
    binds[Core.mod .. " + CONTROL + " .. k] = v
end

-- The built-in scrolling layout (used by special:herdr, special:messaging,
-- special:music -- see peteches.workspaces.*) is a 1-D horizontal tape and
-- doesn't reliably answer the generic movefocus dispatcher for left/right.
-- It has its own layoutmsg focus command instead, so left/right binds have
-- to check the focused workspace's active layout at press-time and pick the
-- dispatcher that layout actually understands. up/down have no meaning on
-- a horizontal tape, so they stay on plain movefocus everywhere.
local function focus_dispatcher(direction)
    if direction == "left" or direction == "right" then
	return function()
	    local ws = hl.get_active_workspace()
	    if ws and ws.tiled_layout == "scrolling" then
		hl.dispatch(hl.dsp.layout("focus " .. (direction == "left" and "l" or "r")))
	    else
		hl.dispatch(hl.dsp.focus({ direction = direction }))
	    end
	end
    end

    return hl.dsp.focus({ direction = direction })
end

for bind, direction in pairs(binds) do
    hl.bind(bind, focus_dispatcher(direction), {
	description = "Focus window " .. direction
    })
end

-- Mouse support is especially useful once floating windows enter the workflow.
hl.bind(Core.mod .. " + mouse:272", hl.dsp.window.drag(), {
  mouse = true,
  description = "Drag active window",
})
