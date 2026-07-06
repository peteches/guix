Lib = require("peteches.keybinds.lib")
-- -----------------------------------------------------------------------------
-- Window command mode: SUPER + Space, w
-- -----------------------------------------------------------------------------

hl.define_submap("leader_window", function()
	hl.bind("f", Lib.dispatch_and_reset(hl.dsp.window.fullscreen("fullscreen", "toggle")), {
		description = "Toggle fullscreen",
	})

	hl.bind("SHIFT + f", Lib.dispatch_and_reset(hl.dsp.window.float({ action = "toggle" })), {
		description = "Toggle floating",
	})

	hl.bind("p", Lib.dispatch_and_reset(hl.dsp.window.pseudo({ action = "toggle" })), {
		description = "Toggle pseudo-tiling",
	})

	hl.bind("c", Lib.dispatch_and_reset(hl.dsp.window.center()), {
		description = "Center active window",
	})

	hl.bind("t", Lib.dispatch_and_reset(hl.dsp.window.alter_zorder({ mode = "top" })), {
		description = "Put active window on top",
	})

	hl.bind("q", Lib.dispatch_and_reset(hl.dsp.window.close()), {
		description = "Close active window",
	})

	hl.bind("m", hl.dsp.submap("move_window"), {
		description = "Move window submap",
	})

	hl.bind("escape", hl.dsp.submap("reset"), {
		description = "reset submaps",
	})
end)

hl.define_submap("move_window",
	function()
		for k, v in pairs(Emacs.movement) do
			hl.bind(k, hl.dsp.window.move({ direction = v }), {
				description = "Move window " .. v,
			})
		end

		hl.bind("s", hl.dsp.submap("swap"), {
			description = "swap windows",
		})
	end)

hl.define_submap("window_swap",
	function()
		for k, v in pairs(Emacs.movement) do
			hl.bind(k, hl.dsp.window.swap({ direction = v }), {
				description = "Swap with window in direction " .. v,
			})
		end
	end)
