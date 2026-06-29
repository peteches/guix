local ok, reason, code = os.execute("command -v beeper 2>&1 >/dev/null")

-- if ok ~= true then
--     hl.notification.create({ text = "missing beeper command", timeout = 30 })
--     return
-- end

-- autostart beeper
hl.on("hyprland.start", function()
			    hl.exec_cmd("beeper")
end)

hl.window_rule({
  match = {
    class = "Beeper", -- verify with: hyprctl clients
  },
  tag = "+messaging",
})






