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
  -- Expose this desktop's per-user PulseAudio session over TCP to
  -- claude-workstation (100.94.152.119) so the VM's `rec` wrapper can
  -- capture this machine's mic for pi-dictate (voice dictation in pi --
  -- see peteches/home/configs/claude-workstation-peteches.scm for the
  -- wrapper and PULSE_SERVER).  The TCP listener is restricted to the
  -- VM's Tailscale IP via auth-ip-acl; the retry loop waits for the
  -- session server to come up (the canberra-gtk-play login sound above
  -- starts it).  Only the session server is affected, not the system
  -- one; the module is loaded once per session.
  hl.exec_cmd("sh -lc 'for i in 1 2 3 4 5 6 7 8 9 10; do pactl load-module module-native-protocol-tcp auth-ip-acl=100.94.152.119 2>/dev/null && break; sleep 3; done'")
  end
)
