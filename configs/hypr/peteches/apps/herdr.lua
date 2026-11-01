-- herdr remote autostart -- INERT, deliberately.
--
-- This used to autostart `herdr --remote claude-workstation.ts': the
-- peteches account on that VM ran a herdr server with 4 standing spaces
-- (guix, bas, and sudo'd panes into criticalgrind/ygo), so one remote window
-- reached every Claude Code account on that box.  Claude Code has come off
-- every machine, claude-workstation is now only its WireGuard/SOCKS5 jump box
-- (see peteches/systems/claude-workstation.scm), and the account there no
-- longer runs a herdr server -- so the exec below is commented out rather
-- than deleted, and points at a host that no longer has a server to accept
-- it.  Nothing else in this file depends on it.
--
-- If herdr gets a server again, the two candidates are: local (this desktop,
-- where pi runs) or `agents.ts', which now has pi under peteches -- the .ts
-- alias comes from (peteches home modules ssh) and herdr resolves it through
-- the normal SSH config, so no herdr-specific connection setup is needed.
--
-- hl.on("hyprland.start", function()
--   hl.exec_cmd("alacritty --class herdr -e herdr --remote claude-workstation.ts")
-- end)

-- The window rule stays active: it still applies to any herdr window opened
-- by hand (or by a future autostart), which is what the +herdr tag is used
-- for elsewhere in the hypr config.
hl.window_rule({
  match = {
    class = "herdr", -- verify with: hyprctl clients
  },
  tag = "+herdr",
})
