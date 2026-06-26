lib = require("peteches.keybinds.lib")
Local = require("peteches.locals")
-- -----------------------------------------------------------------------------
-- AI Mode, all my ai related stuff
--
-- s - switch ai model on nug
-- -----------------------------------------------------------------------------

hl.define_submap("leader_resize", function()
  hl.bind("s", lib.dispatch_and_reset(hl.dsp.exec_cmd(Local.ai_switch_cmd)), {
    description = "Switch AI model on server",
  })

  hl.bind("escape", hl.dsp.submap("reset"), {
      description = "reset submaps",
  })

end)

