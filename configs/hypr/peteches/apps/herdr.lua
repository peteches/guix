-- autostart three herdr remote sessions, one per claude-workstation
-- account, each in its own alacritty window.  Host aliases below are the
-- .ts ones defined in (peteches home modules ssh) -- see ssh.scm's
-- claude-workstation.ts / claude-workstation-cg.ts /
-- claude-workstation-ygo.ts entries (peteches / criticalgrind / ygo
-- respectively), which resolve over Tailscale rather than the LAN IP.
-- herdr resolves these through the normal SSH config, so no extra
-- herdr-specific connection setup is needed.
local accounts = {
  "claude-workstation.ts",     -- peteches
  "claude-workstation-cg.ts",  -- criticalgrind
  "claude-workstation-ygo.ts", -- ygo
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
