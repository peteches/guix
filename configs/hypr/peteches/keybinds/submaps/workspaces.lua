Lib = require("peteches.keybinds.lib")

hl.define_submap("leader_workspaces",
	function()
		hl.bind("m", Lib.dispatch_and_reset(
			hl.dsp.workspace.toggle_special("messaging")
		), {
			description = "Toggle messaging workspace",
		})

		hl.bind("Return", hl.dsp.submap("reset"), {
			description = "Exit resize mode",
		})

		hl.bind("escape", hl.dsp.submap("reset"), {
			description = "reset submaps",
		})
	end)
