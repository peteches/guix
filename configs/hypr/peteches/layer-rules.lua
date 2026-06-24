-- Layer-shell surfaces get the frosted/translucent treatment.  Check
-- `hyprctl layers` if a program changes namespace in a future release.

hl.layer_rule({
  name = "dms-layer-polish",
  match = { namespace = "^(dms)$" },
  no_anim = true,
  blur = true,
  blur_popups = true,
  ignore_alpha = 0.55,
})

hl.layer_rule({
  name = "wofi-frosted-launcher",
  match = { namespace = "^(wofi)$" },
  blur = true,
  blur_popups = true,
  ignore_alpha = 0.55,
  dim_around = true,
  animation = "popin 80%",
})
