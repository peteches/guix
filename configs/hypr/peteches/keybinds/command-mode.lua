Lib = require("peteches.keybinds.lib")
Core = require("peteches.keybinds.core")
require("peteches.keybinds.submaps.window")
require("peteches.keybinds.submaps.monitor")
require("peteches.keybinds.submaps.scratchpad")
require("peteches.keybinds.submaps.resize")
require("peteches.keybinds.submaps.groups")
require("peteches.keybinds.submaps.ai")

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
  hl.bind("w", hl.dsp.submap("leader_window"), {
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

  hl.bind("g", hl.dsp.submap("leader_group"), {
    description = "Group/tabbed-window commands",
  })

  hl.bind("q", hl.dsp.submap("reset"), {
    description = "Cancel command mode",
  })

  hl.bind("escape", hl.dsp.submap("reset"), {
    description = "Cancel command mode",
  })
end)
