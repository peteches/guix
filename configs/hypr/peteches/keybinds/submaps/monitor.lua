lib = require("peteches.keybinds.lib")
-- -----------------------------------------------------------------------------
-- Monitor command mode: SUPER + Space, m
--
-- Uses Emacs-ish b/f/p/n semantics:
-- - f/n go forward/next
-- - b/p go backward/previous
-- -----------------------------------------------------------------------------

hl.define_submap("leader_monitor", function()
  hl.bind("n", lib.dispatch_and_reset(hl.dsp.focus({ monitor = "+1" })), {
    description = "Focus next monitor",
  })

  hl.bind("p", lib.dispatch_and_reset(hl.dsp.focus({ monitor = "-1" })), {
    description = "Focus previous monitor",
  })

  hl.bind("f", lib.dispatch_and_reset(hl.dsp.window.move({ monitor = "+1", follow = true })), {
    description = "Move window to next monitor and follow",
  })

  hl.bind("b", lib.dispatch_and_reset(hl.dsp.window.move({ monitor = "-1", follow = true })), {
    description = "Move window to previous monitor and follow",
  })

  hl.bind("escape", hl.dsp.submap("reset"), {
    description = "reset submaps",
  })

end)
