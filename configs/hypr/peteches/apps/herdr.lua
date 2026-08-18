-- autostart three herdr remote sessions, one per claude-workstation
-- account, each in its own alacritty window.  Host aliases below are the
-- ones defined in (peteches home modules ssh) -- see ssh.scm's
-- claude-workstation / claude-workstation-cg / claude-workstation-ygo
-- entries (peteches / criticalgrind / ygo respectively). herdr resolves
-- these through the normal SSH config, so no extra herdr-specific
-- connection setup is needed.
local accounts = {
  "claude-workstation",     -- peteches
  "claude-workstation-cg",  -- criticalgrind
  "claude-workstation-ygo", -- ygo
}

hl.on("hyprland.start", function()
  for _, host in ipairs(accounts) do
    hl.exec_cmd("alacritty --class herdr -e herdr --remote " .. host)
  end
end)

hl.window_rule({
  match = {
    class = "herdr", -- verify with: hyprctl clients
  },
  tag = "+herdr",
})
