-- -----------------------------------------------------------------------------
-- Multimedia / hardware keys
-- -----------------------------------------------------------------------------

hl.bind("XF86AudioRaiseVolume", hl.dsp.exec_cmd("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+"), {
  repeating = true,
  description = "Raise volume",
})

hl.bind("XF86AudioLowerVolume", hl.dsp.exec_cmd("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"), {
  repeating = true,
  description = "Lower volume",
})

hl.bind("XF86AudioMute", hl.dsp.exec_cmd("wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"), {
  locked = true,
  description = "Mute volume",
})

-- Requires playerctl.
hl.bind("XF86AudioPlay", hl.dsp.exec_cmd("playerctl play-pause"), {
  locked = true,
  description = "Play/pause media",
})

hl.bind("XF86AudioPrev", hl.dsp.exec_cmd("playerctl previous"), {
  locked = true,
  description = "Previous media track",
})

hl.bind("XF86AudioNext", hl.dsp.exec_cmd("playerctl next"), {
  locked = true,
  description = "Next media track",
})

hl.bind("XF86MonBrightnessDown", hl.dsp.exec_cmd("brightnessctl s 5%-"), {
  description = "Lower display brightness",
})

hl.bind("XF86MonBrightnessUp", hl.dsp.exec_cmd("brightnessctl s 5%+"), {
  description = "Raise display brightness",
})

hl.bind("SHIFT + XF86MonBrightnessDown", hl.dsp.exec_cmd("brightnessctl -d '*::kbd_backlight' s 5%-"), {
  description = "Lower keyboard backlight",
})

hl.bind("SHIFT + XF86MonBrightnessUp", hl.dsp.exec_cmd("brightnessctl -d '*::kbd_backlight' s 5%+"), {
  description = "Raise keyboard backlight",
})
