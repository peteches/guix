hl.on("hyprland.start", function()
  hl.exec_cmd("dms run -c ~/.guix-home/profile/share/quickshell")
  hl.exec_cmd("emacs --fg-daemon >> ${XDG_LOG_HOME:-$HOME/.local/var/log}/emacs-daemon.log 2>&1")
  hl.exec_cmd("mako")
  hl.exec_cmd("canberra-gtk-play -i desktop-login")
  end
)