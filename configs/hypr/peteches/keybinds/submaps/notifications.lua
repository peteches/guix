Lib = require("peteches.keybinds.lib")

-- -----------------------------------------------------------------------------
-- Notification command mode: SUPER + Space, n
--
-- Mako targets the first/current notification when no -n <id> is supplied.
-- `invoke` runs the default action, which is what makes app-provided
-- "jump/open/focus this" notifications useful from the keyboard.
-- -----------------------------------------------------------------------------

local action_menu_cmd = "makoctl menu -- wofi -d -p 'Notification action: '"

hl.define_submap("leader_notifications", function()
  hl.bind("Return", Lib.dispatch_and_reset(hl.dsp.exec_cmd("makoctl invoke")), {
    description = "Invoke current notification default action",
  })

  hl.bind("i", Lib.dispatch_and_reset(hl.dsp.exec_cmd("makoctl invoke")), {
    description = "Invoke current notification default action",
  })

  hl.bind("a", Lib.dispatch_and_reset(hl.dsp.exec_cmd(action_menu_cmd)), {
    description = "Choose current notification action",
  })

  hl.bind("d", Lib.dispatch_and_reset(hl.dsp.exec_cmd("makoctl dismiss")), {
    description = "Dismiss current notification",
  })

  hl.bind("SHIFT + d", Lib.dispatch_and_reset(hl.dsp.exec_cmd("makoctl dismiss --all")), {
    description = "Dismiss all notifications",
  })

  hl.bind("r", Lib.dispatch_and_reset(hl.dsp.exec_cmd("makoctl restore")), {
    description = "Restore last dismissed notification",
  })

  hl.bind("m", Lib.dispatch_and_reset(hl.dsp.exec_cmd("makoctl mode -t do-not-disturb")), {
    description = "Toggle notification do-not-disturb mode",
  })

  hl.bind("l", Lib.dispatch_and_reset(hl.dsp.exec_cmd("makoctl list | wofi --dmenu -p 'Notifications: '")), {
    description = "List current notifications",
  })

  hl.bind("h", Lib.dispatch_and_reset(hl.dsp.exec_cmd("makoctl history | wofi --dmenu -p 'Notification history: '")), {
    description = "List notification history",
  })

  hl.bind("escape", hl.dsp.submap("reset"), {
    description = "reset submaps",
  })
end)
