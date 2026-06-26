Lib = require("peteches.keybinds.lib")
-- -----------------------------------------------------------------------------
-- Group command mode: SUPER + Space, g
--
-- Groups are Hyprland's tabbed-window/container workflow. This is optional, but
-- it is exactly the kind of feature that belongs in command mode while you test it.
-- -----------------------------------------------------------------------------

hl.define_submap("leader_group", function()
  hl.bind("g", Lib.dispatch_and_reset(hl.dsp.group.toggle()), {
    description = "Toggle window group",
  })

  hl.bind("l", Lib.dispatch_and_reset(hl.dsp.group.lock({ action = "toggle" })), {
    description = "Toggle group lock",
  })

  hl.bind("n", Lib.dispatch_and_reset(hl.dsp.group.next()), {
    description = "Next window in group",
  })

  hl.bind("p", Lib.dispatch_and_reset(hl.dsp.group.prev()), {
    description = "Previous window in group",
  })

  hl.bind("f", Lib.dispatch_and_reset(hl.dsp.group.move_window({ forward = true })), {
    description = "Move window forward in group",
  })

  hl.bind("b", Lib.dispatch_and_reset(hl.dsp.group.move_window({ forward = false })), {
    description = "Move window backward in group",
  })

  hl.bind("escape", hl.dsp.submap("reset"), {
    description = "reset submaps",
  })

end)
