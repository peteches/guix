Lib = require("peteches.keybinds.lib")

hl.define_submap("leader_layout",
	function()
		hl.bind("t", Lib.dispatch_and_reset(hl.dsp.layout("togglesplit")), {
			description = "Toggle Split direction",
		})
		hl.bind("escape", hl.dsp.submap("reset"), {
			description = "reset submaps",
		})
	end)
