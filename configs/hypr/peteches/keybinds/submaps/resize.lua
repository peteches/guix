Lib = require("peteches.keybinds.lib")
-- -----------------------------------------------------------------------------
-- Resize mode: SUPER + Space, r
--
-- b/f/p/n keep their Emacs direction meaning, but resize is relative:
-- - b/f shrink/grow width
-- - p/n shrink/grow height
-- -----------------------------------------------------------------------------

hl.define_submap("leader_resize", function()
  hl.bind("b", hl.dsp.window.resize({ x = -40, y = 0, relative = true }), {
    repeating = true,
    description = "Shrink width",
  })

  hl.bind("f", hl.dsp.window.resize({ x = 40, y = 0, relative = true }), {
    repeating = true,
    description = "Grow width",
  })

  hl.bind("p", hl.dsp.window.resize({ x = 0, y = -40, relative = true }), {
    repeating = true,
    description = "Shrink height",
  })

  hl.bind("n", hl.dsp.window.resize({ x = 0, y = 40, relative = true }), {
    repeating = true,
    description = "Grow height",
  })

  hl.bind("Return", hl.dsp.submap("reset"), {
    description = "Exit resize mode",
  })

  hl.bind("escape", hl.dsp.submap("reset"), {
      description = "reset submaps",
  })

end)

