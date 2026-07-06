Lib = require("peteches.keybinds.lib")
Core = require("peteches.keybinds.core")
require("peteches.keybinds.submaps.window")
require("peteches.keybinds.submaps.monitor")
require("peteches.keybinds.submaps.scratchpad")
require("peteches.keybinds.submaps.resize")
require("peteches.keybinds.submaps.groups")
require("peteches.keybinds.submaps.workspaces")
require("peteches.keybinds.submaps.ai")
require("peteches.keybinds.submaps.launcher")
require("peteches.keybinds.submaps.notifications")

-- -----------------------------------------------------------------------------
-- Command mode
--
-- SUPER + Space enters a modal WM command layer. This is the main workflow
-- addition: uncommon WM actions are grouped by purpose instead of becoming
-- awkward global chords.
-- -----------------------------------------------------------------------------

hl.bind(Core.leader, hl.dsp.submap("leader"), {
	description = "Enter Hyprland command mode",
})

hl.define_submap("leader", function()
	hl.bind("a", hl.dsp.submap("leader_ai"), {
		description = "AI commands",
	})
	hl.bind("SHIFT + w", hl.dsp.submap("leader_window"), {
		description = "Window commands",
	})

	hl.bind("m", hl.dsp.submap("leader_monitor"), {
		description = "Monitor commands",
	})

	hl.bind("s", hl.dsp.submap("leader_scratch"), {
		description = "Scratchpad commands",
	})

	hl.bind("r", hl.dsp.submap("leader_resize"), {
		description = "Resize mode",
	})

	hl.bind("n", hl.dsp.submap("leader_notifications"), {
		description = "Notification commands",
	})

	hl.bind("g", hl.dsp.submap("leader_group"), {
		description = "Group/tabbed-window commands",
	})

	hl.bind("l", hl.dsp.submap("leader_launcher"), {
		description = "Launch Apps/Programs",
	})

	hl.bind("q", hl.dsp.submap("reset"), {
		description = "Cancel command mode",
	})

	hl.bind("w", hl.dsp.submap("leader_workspaces"), {
		description = "More involved workspace maangement"
	})

	hl.bind("escape", hl.dsp.submap("reset"), {
		description = "Cancel command mode",
	})
end)
