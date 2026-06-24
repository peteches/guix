local home = os.getenv("HOME") or "/home/peteches"
local guix_home_profile = home .. "/.guix-home/profile"
local dms_bin = guix_home_profile .. "/bin/dms"
local quickshell_dir = guix_home_profile .. "/share/quickshell"
hl.on("hyprland.start", function()
  hl.exec_cmd(dms_bin .. " run -c " .. quickshell_dir)
  hl.exec_cmd("sh -lc 'state_dir=\"${XDG_STATE_HOME:-$HOME/.local/state}/peteches\"; mkdir -p \"$state_dir\"; sleep 5; \"$HOME/.guix-home/profile/bin/dms-random-wallpaper\" --no-save >> \"$state_dir/dms-random-wallpaper-startup.log\" 2>&1 || true'")
  hl.exec_cmd("emacs --fg-daemon >> ${XDG_LOG_HOME:-$HOME/.local/var/log}/emacs-daemon.log 2>&1")
  hl.exec_cmd("canberra-gtk-play -i desktop-login")
  end
)
