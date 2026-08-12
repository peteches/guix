-- autostart feishin
hl.on("hyprland.start", function()
			    hl.exec_cmd("feishin")
end)

hl.window_rule({
  match = {
    class = "feishin",
  },
  tag = "+music",
})
