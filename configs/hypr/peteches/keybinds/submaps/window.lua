lib = require("peteches.keybinds.lib")
-- -----------------------------------------------------------------------------
-- Window command mode: SUPER + Space, w
-- -----------------------------------------------------------------------------

hl.define_submap("leader_window", function()
  hl.bind("f", lib.dispatch_and_reset(hl.dsp.window.fullscreen("fullscreen", "toggle")), {
    description = "Toggle fullscreen",
  })

  hl.bind("SHIFT + f", lib.dispatch_and_reset(hl.dsp.window.float({ action = "toggle" })), {
    description = "Toggle floating",
  })

  hl.bind("p", lib.dispatch_and_reset(hl.dsp.window.pseudo({ action = "toggle" })), {
    description = "Toggle pseudo-tiling",
  })

  hl.bind("c", lib.dispatch_and_reset(hl.dsp.window.center()), {
    description = "Center active window",
  })

  hl.bind("t", lib.dispatch_and_reset(hl.dsp.window.alter_zorder({ mode = "top" })), {
    description = "Put active window on top",
  })

  hl.bind("q", lib.dispatch_and_reset(hl.dsp.window.close()), {
    description = "Close active window",
  })

  hl.bind("m", hl.dsp.submap("move_window"), {
    description = "Move window submap",
  })
  
  hl.bind("escape", hl.dsp.submap("reset"), {
    description = "reset submaps",
  })

end)

hl.define_submap("move_window",
		 function()
		     hl.bind("b", hl.dsp.window.move({ direction = "left" }), {
			 description = "Move window left",
		     })
		     
		     hl.bind("left", hl.dsp.window.move({ direction = "left" }), {
			 description = "Move window left",
		     })

		     hl.bind("f", hl.dsp.window.move({ direction = "right" }), {
			 description = "Move window right",
		     })
		     hl.bind("right", hl.dsp.window.move({ direction = "right" }), {
			 description = "Move window right",
		     })

		     hl.bind("p", hl.dsp.window.move({ direction = "up" }), {
			 description = "Move window up",
		     })
		     hl.bind("up", hl.dsp.window.move({ direction = "up" }), {
			 description = "Move window up",
		     })

		     hl.bind("n", hl.dsp.window.move({ direction = "down" }), {
			 description = "Move window down",
		     })
		     hl.bind("down", hl.dsp.window.move({ direction = "down" }), {
			 description = "Move window down",
		     })
		 end)
