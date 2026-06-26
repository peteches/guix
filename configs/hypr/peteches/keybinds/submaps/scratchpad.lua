Lib = require("peteches.keybinds.lib")
-- -----------------------------------------------------------------------------
-- Scratchpad command mode: SUPER + Space, s
--
-- The named special workspace acts as a scratchpad visible from any monitor.
-- -----------------------------------------------------------------------------

hl.define_submap("leader_scratch", function()
  hl.bind("s", Lib.dispatch_and_reset(hl.dsp.workspace.toggle_special("scratch")), {
    description = "Toggle scratchpad",
  })

  hl.bind("m", Lib.dispatch_and_reset(hl.dsp.window.move({ workspace = "special:scratch" })), {
    description = "Move active window to scratchpad",
  })

  hl.bind("t", Lib.dispatch_and_reset(hl.dsp.exec_cmd(Local.term_cmd, {
    workspace = "special:scratch",
  })), {
    description = "Open terminal on scratchpad",
  })

  hl.bind("p", Lib.dispatch_and_reset(hl.dsp.exec_cmd(Local.password_lookup_cmd, {
    workspace = "special:scratch",
  })), {
    description = "Open password lookup on scratchpad",
  })

  hl.bind("escape", hl.dsp.submap("reset"), {
    description = "reset submaps",
  })

end)
