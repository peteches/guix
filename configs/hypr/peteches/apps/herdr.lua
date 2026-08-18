-- autostart a single herdr remote session against the peteches account on
-- claude-workstation. That account's herdr server keeps 4 standing spaces
-- (guix, bas, and sudo'd panes into criticalgrind/ygo) -- see
-- herdr-spaces-bootstrap-script in (peteches home modules
-- claude-workstation) and %peteches-herdr-spaces in
-- peteches/home/configs/claude-workstation-peteches.scm -- so this one
-- remote reaches every account; there is no separate autostart for
-- criticalgrind/ygo any more.
--
-- "claude-workstation.ts" is the .ts alias defined in (peteches home
-- modules ssh), resolving over Tailscale rather than the LAN IP. herdr
-- resolves it through the normal SSH config, so no extra herdr-specific
-- connection setup is needed.
hl.on("hyprland.start", function()
  hl.exec_cmd("alacritty --class herdr -e herdr --remote claude-workstation.ts")
end)

hl.window_rule({
  match = {
    class = "herdr", -- verify with: hyprctl clients
  },
  tag = "+herdr",
})
