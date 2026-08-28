-- Dagon's actual outputs (HDMI-A-1: PHL 32E1N1800LA, DP-1: VSC VP3268a-4K)
-- are its own — this used to be a straight copy of nug.lua, which hardcoded
-- nug's DP-1/DP-2/DP-3 layout and happened to disable dagon's real DP-1 as
-- a side effect (same port name, different monitor).
--
-- VSC VP3268a-4K (DP-1) is primary and sits at the origin; PHL 32E1N1800LA
-- (HDMI-A-1) is physically mounted above it, so it gets a negative Y offset
-- equal to its own height to stack directly on top, left edges flush.
--
-- Both run unscaled (scale = 1). A 1.5x scale was tried (86abfcd) but
-- caused a flood of "rbo: glCheckFramebufferStatus failed" errors and a
-- full Hyprland deadlock when the config was hot-reloaded live (confirmed
-- 2026-08-28: compositor wedged mid xdg_output-head update, unresponsive
-- for 40+ minutes despite the watchdog thread staying up). Back to native
-- pixel density until that's understood.
hl.monitor({
  output = "DP-1",
  mode = "3840x2160@60.00Hz",
  position = "0x0",
  scale = 1,
  })

hl.monitor({
  output = "HDMI-A-1",
  mode = "3840x2160@60.00Hz",
  position = "0x-2160",
  scale = 1,
  })
