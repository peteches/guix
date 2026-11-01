local home = os.getenv("HOME") or "/home/peteches"
local guix_home_profile = home .. "/.guix-home/profile"
local dms_bin = guix_home_profile .. "/bin/dms"
local mako_bin = guix_home_profile .. "/bin/mako"
local quickshell_dir = guix_home_profile .. "/share/quickshell"
hl.on("hyprland.start", function()
  -- Start Mako first so it claims org.freedesktop.Notifications before DMS.
  hl.exec_cmd("sh -lc 'if [ -x " .. mako_bin .. " ] && ! pgrep -x mako >/dev/null 2>&1; then exec " .. mako_bin .. "; fi'")
  hl.exec_cmd("sh -lc 'sleep 0.5; exec " .. dms_bin .. " run -c " .. quickshell_dir .. "'")
  hl.exec_cmd("sh -lc 'state_dir=\"${XDG_STATE_HOME:-$HOME/.local/state}/peteches\"; mkdir -p \"$state_dir\"; sleep 5; \"$HOME/.guix-home/profile/bin/dms-random-wallpaper\" --no-save >> \"$state_dir/dms-random-wallpaper-startup.log\" 2>&1 || true'")
  hl.exec_cmd("emacs --fg-daemon >> ${XDG_LOG_HOME:-$HOME/.local/var/log}/emacs-daemon.log 2>&1")
  hl.exec_cmd("canberra-gtk-play -i desktop-login")
  -- INERT, deliberately: this used to expose this desktop's per-user
  -- PulseAudio session over TCP to claude-workstation so the VM's `rec'
  -- wrapper could capture this machine's mic for dictation on that box.
  -- Claude Code -- and with it the rec wrapper, its PULSE_SERVER override
  -- and the VM-side firewall rule that let this desktop in -- is gone.
  -- Dictation happens on this desktop through configs/dms/plugins/dictate,
  -- which uses the local PulseAudio session directly.  The auth-ip-acl
  -- address is stale as well (that tailnet node is 100.75.14.28 now), so
  -- this listener would serve nobody.  Commented out rather than deleted
  -- so the mechanism stays findable if another VM needs a network mic.
  -- hl.exec_cmd("sh -lc 'for i in 1 2 3 4 5 6 7 8 9 10; do pactl load-module module-native-protocol-tcp auth-ip-acl=100.94.152.119 2>/dev/null && break; sleep 3; done'")
  end
)
