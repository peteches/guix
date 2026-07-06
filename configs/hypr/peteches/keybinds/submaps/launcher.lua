Lib = require("peteches.keybinds.lib")

hl.define_submap("leader_launcher",
	function()
		hl.bind("e", Lib.dispatch_and_reset(hl.dsp.exec_cmd(Local.editor_cmd)), {
			description = "Open editor",
		})

		hl.bind("b", Lib.dispatch_and_reset(hl.dsp.exec_cmd(Local.browser_cmd)), {
			description = "Open browser",
		})

		hl.bind("d", Lib.dispatch_and_reset(hl.dsp.exec_cmd("wofi --show drun")), {
			description = "Open application launcher",
		})


		hl.bind("s", Lib.dispatch_and_reset(hl.dsp.exec_cmd(Local.screenshot_cmd)), {
			description = "Screenshot",
		})


		hl.bind("w", Lib.dispatch_and_reset(hl.dsp.exec_cmd(Local.random_wallpaper_cmd)), {
			description = "Choose random wallpaper",
		})
	end)
