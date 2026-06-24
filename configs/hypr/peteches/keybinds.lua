-- Hyprland keybinds: command-mode oriented v1
--
-- Design goals:
-- - Keep direct binds only for high-frequency actions.
-- - Keep Emacs-style navigation: b/f/p/n = left/right/up/down.
-- - Put lower-frequency WM operations behind SUPER + Space submaps.
-- - Make window-to-workspace actions follow the moved window.
-- - Stay portable across laptop, desktop, and multi-monitor systems.

Local = require("peteches.locals")

local mod = "SUPER"
local leader = mod .. " + Space"

local function reset_submap()
  return hl.dsp.submap("reset")
end

local function dispatch_and_reset(dispatcher)
  return function()
    hl.dispatch(dispatcher)
    hl.dispatch(reset_submap())
  end
end

local function dispatch_many_and_reset(dispatchers)
  return function()
    for _, dispatcher in ipairs(dispatchers) do
      hl.dispatch(dispatcher)
    end
    hl.dispatch(reset_submap())
  end
end

local function bind_workspace(key, workspace)
  hl.bind(mod .. " + " .. key, hl.dsp.focus({
    workspace = workspace,
    on_current_monitor = true,
  }), {
    description = "Focus workspace " .. workspace .. " on current monitor",
  })

  hl.bind(mod .. " + SHIFT + " .. key, hl.dsp.window.move({
    workspace = workspace,
    follow = true,
  }), {
    description = "Move window to workspace " .. workspace .. " and follow",
  })
end

-- -----------------------------------------------------------------------------
-- Direct high-frequency binds
-- -----------------------------------------------------------------------------

hl.bind(mod .. " + Return", hl.dsp.exec_cmd(Local.term_cmd), {
  description = "Open terminal",
})

hl.bind(mod .. " + e", hl.dsp.exec_cmd(Local.editor_cmd), {
  description = "Open editor",
})

hl.bind(mod .. " + b", hl.dsp.exec_cmd(Local.browser_cmd), {
  description = "Open browser",
})

hl.bind(mod .. " + d", hl.dsp.exec_cmd("wofi --show drun"), {
  description = "Open application launcher",
})

hl.bind(mod .. " + p", hl.dsp.exec_cmd(Local.password_lookup_cmd), {
  description = "Password lookup",
})

hl.bind("Print", hl.dsp.exec_cmd(Local.screenshot_cmd), {
  description = "Screenshot",
})

hl.bind(mod .. " + SHIFT + s", hl.dsp.exec_cmd(Local.screenshot_cmd), {
  description = "Screenshot",
})

hl.bind(mod .. " + SHIFT + d", hl.dsp.exec_cmd("dms ipc call notifications dismissAllPopups"), {
  description = "Dismiss notification popups",
})

hl.bind(mod .. " + SHIFT + w", hl.dsp.exec_cmd(Local.random_wallpaper_cmd), {
  description = "Choose random wallpaper",
})

hl.bind(mod .. " + Tab", hl.dsp.focus({ last = true }), {
  description = "Focus last window",
})

hl.bind(mod .. " + q", hl.dsp.window.close(), {
  description = "Close active window",
})

hl.bind(mod .. " + f", hl.dsp.window.fullscreen("fullscreen", "toggle"), {
  description = "Toggle fullscreen",
})

-- Universal escape hatch for any command-mode submap.
hl.bind(mod .. " + escape", reset_submap(), {
  submap_universal = true,
  description = "Reset Hyprland keymap",
})

-- -----------------------------------------------------------------------------
-- Emacs-style direct window navigation
-- -----------------------------------------------------------------------------

hl.bind(mod .. " + CONTROL + b", hl.dsp.focus({ direction = "left" }), {
  description = "Focus window left",
})

hl.bind(mod .. " + CONTROL + f", hl.dsp.focus({ direction = "right" }), {
  description = "Focus window right",
})

hl.bind(mod .. " + CONTROL + p", hl.dsp.focus({ direction = "up" }), {
  description = "Focus window up",
})

hl.bind(mod .. " + CONTROL + n", hl.dsp.focus({ direction = "down" }), {
  description = "Focus window down",
})

hl.bind(mod .. " + SHIFT + CONTROL + b", hl.dsp.window.move({ direction = "left" }), {
  description = "Move window left",
})

hl.bind(mod .. " + SHIFT + CONTROL + f", hl.dsp.window.move({ direction = "right" }), {
  description = "Move window right",
})

hl.bind(mod .. " + SHIFT + CONTROL + p", hl.dsp.window.move({ direction = "up" }), {
  description = "Move window up",
})

hl.bind(mod .. " + SHIFT + CONTROL + n", hl.dsp.window.move({ direction = "down" }), {
  description = "Move window down",
})

-- Mouse support is especially useful once floating windows enter the workflow.
hl.bind(mod .. " + mouse:272", hl.dsp.window.drag(), {
  mouse = true,
  description = "Drag active window",
})

hl.bind(mod .. " + mouse:273", hl.dsp.window.resize(), {
  mouse = true,
  description = "Resize active window",
})

-- -----------------------------------------------------------------------------
-- Workspaces
--
-- Hyprland numeric workspace IDs start at 1. The physical 0 key is mapped to
-- workspace 10 rather than workspace 0.
-- -----------------------------------------------------------------------------

for i = 1, 9 do
  bind_workspace(tostring(i), i)
end
bind_workspace("0", 10)

-- -----------------------------------------------------------------------------
-- Command mode
--
-- SUPER + Space enters a modal WM command layer. This is the main workflow
-- addition: uncommon WM actions are grouped by purpose instead of becoming
-- awkward global chords.
-- -----------------------------------------------------------------------------

hl.bind(leader, hl.dsp.submap("leader"), {
  description = "Enter Hyprland command mode",
})

hl.define_submap("leader", function()
  hl.bind("w", hl.dsp.submap("leader_window"), {
    description = "Window commands",
  })

  hl.bind("m", hl.dsp.submap("leader_monitor"), {
    description = "Monitor commands",
  })

  hl.bind("s", hl.dsp.submap("leader_scratch"), {
    description = "Scratchpad commands",
  })

  hl.bind("r", hl.dsp.submap("leader_resize"), {
    description = "Resize mode",
  })

  hl.bind("g", hl.dsp.submap("leader_group"), {
    description = "Group/tabbed-window commands",
  })

  hl.bind("a", dispatch_and_reset(hl.dsp.exec_cmd(Local.ai_switch_cmd)), {
    description = "Switch AI model on server",
  })

  hl.bind("q", reset_submap(), {
    description = "Cancel command mode",
  })

  hl.bind("escape", reset_submap(), {
    description = "Cancel command mode",
  })
end)

-- -----------------------------------------------------------------------------
-- Window command mode: SUPER + Space, w
-- -----------------------------------------------------------------------------

hl.define_submap("leader_window", function()
  hl.bind("f", dispatch_and_reset(hl.dsp.window.fullscreen("fullscreen", "toggle")), {
    description = "Toggle fullscreen",
  })

  hl.bind("m", dispatch_and_reset(hl.dsp.window.fullscreen("maximized", "toggle")), {
    description = "Toggle maximized fullscreen mode",
  })

  hl.bind("SHIFT + f", dispatch_and_reset(hl.dsp.window.float({ action = "toggle" })), {
    description = "Toggle floating",
  })

  hl.bind("p", dispatch_and_reset(hl.dsp.window.pseudo({ action = "toggle" })), {
    description = "Toggle pseudo-tiling",
  })

  hl.bind("c", dispatch_and_reset(hl.dsp.window.center()), {
    description = "Center active window",
  })

  hl.bind("t", dispatch_and_reset(hl.dsp.window.alter_zorder({ mode = "top" })), {
    description = "Put active window on top",
  })

  hl.bind("q", dispatch_and_reset(hl.dsp.window.close()), {
    description = "Close active window",
  })

  hl.bind("escape", hl.dsp.submap("leader"), {
    description = "Back to command mode",
  })

  hl.bind("SHIFT + escape", reset_submap(), {
    description = "Exit command mode",
  })
end)

-- -----------------------------------------------------------------------------
-- Monitor command mode: SUPER + Space, m
--
-- Uses Emacs-ish b/f/p/n semantics:
-- - f/n go forward/next
-- - b/p go backward/previous
-- -----------------------------------------------------------------------------

hl.define_submap("leader_monitor", function()
  hl.bind("n", dispatch_and_reset(hl.dsp.focus({ monitor = "+1" })), {
    description = "Focus next monitor",
  })

  hl.bind("p", dispatch_and_reset(hl.dsp.focus({ monitor = "-1" })), {
    description = "Focus previous monitor",
  })

  hl.bind("f", dispatch_and_reset(hl.dsp.window.move({ monitor = "+1", follow = true })), {
    description = "Move window to next monitor and follow",
  })

  hl.bind("b", dispatch_and_reset(hl.dsp.window.move({ monitor = "-1", follow = true })), {
    description = "Move window to previous monitor and follow",
  })

  hl.bind("escape", hl.dsp.submap("leader"), {
    description = "Back to command mode",
  })

  hl.bind("SHIFT + escape", reset_submap(), {
    description = "Exit command mode",
  })
end)

-- -----------------------------------------------------------------------------
-- Scratchpad command mode: SUPER + Space, s
--
-- The named special workspace acts as a scratchpad visible from any monitor.
-- -----------------------------------------------------------------------------

hl.define_submap("leader_scratch", function()
  hl.bind("s", dispatch_and_reset(hl.dsp.workspace.toggle_special("scratch")), {
    description = "Toggle scratchpad",
  })

  hl.bind("m", dispatch_and_reset(hl.dsp.window.move({ workspace = "special:scratch" })), {
    description = "Move active window to scratchpad",
  })

  hl.bind("t", dispatch_and_reset(hl.dsp.exec_cmd(Local.term_cmd, {
    workspace = "special:scratch",
  })), {
    description = "Open terminal on scratchpad",
  })

  hl.bind("p", dispatch_and_reset(hl.dsp.exec_cmd(Local.password_lookup_cmd, {
    workspace = "special:scratch",
  })), {
    description = "Open password lookup on scratchpad",
  })

  hl.bind("escape", hl.dsp.submap("leader"), {
    description = "Back to command mode",
  })

  hl.bind("SHIFT + escape", reset_submap(), {
    description = "Exit command mode",
  })
end)

-- -----------------------------------------------------------------------------
-- Resize mode: SUPER + Space, r
--
-- b/f/p/n keep their Emacs direction meaning, but resize is relative:
-- - b/f shrink/grow width
-- - p/n shrink/grow height
-- -----------------------------------------------------------------------------

hl.define_submap("leader_resize", function()
  hl.bind("b", hl.dsp.window.resize({ x = -40, y = 0, relative = true }), {
    repeating = true,
    description = "Shrink width",
  })

  hl.bind("f", hl.dsp.window.resize({ x = 40, y = 0, relative = true }), {
    repeating = true,
    description = "Grow width",
  })

  hl.bind("p", hl.dsp.window.resize({ x = 0, y = -40, relative = true }), {
    repeating = true,
    description = "Shrink height",
  })

  hl.bind("n", hl.dsp.window.resize({ x = 0, y = 40, relative = true }), {
    repeating = true,
    description = "Grow height",
  })

  hl.bind("Return", reset_submap(), {
    description = "Exit resize mode",
  })

  hl.bind("escape", hl.dsp.submap("leader"), {
    description = "Back to command mode",
  })

  hl.bind("SHIFT + escape", reset_submap(), {
    description = "Exit command mode",
  })
end)

-- -----------------------------------------------------------------------------
-- Group command mode: SUPER + Space, g
--
-- Groups are Hyprland's tabbed-window/container workflow. This is optional, but
-- it is exactly the kind of feature that belongs in command mode while you test it.
-- -----------------------------------------------------------------------------

hl.define_submap("leader_group", function()
  hl.bind("g", dispatch_and_reset(hl.dsp.group.toggle()), {
    description = "Toggle window group",
  })

  hl.bind("l", dispatch_and_reset(hl.dsp.group.lock({ action = "toggle" })), {
    description = "Toggle group lock",
  })

  hl.bind("n", dispatch_and_reset(hl.dsp.group.next()), {
    description = "Next window in group",
  })

  hl.bind("p", dispatch_and_reset(hl.dsp.group.prev()), {
    description = "Previous window in group",
  })

  hl.bind("f", dispatch_and_reset(hl.dsp.group.move_window({ forward = true })), {
    description = "Move window forward in group",
  })

  hl.bind("b", dispatch_and_reset(hl.dsp.group.move_window({ forward = false })), {
    description = "Move window backward in group",
  })

  hl.bind("escape", hl.dsp.submap("leader"), {
    description = "Back to command mode",
  })

  hl.bind("SHIFT + escape", reset_submap(), {
    description = "Exit command mode",
  })
end)

-- -----------------------------------------------------------------------------
-- Multimedia / hardware keys
-- -----------------------------------------------------------------------------

hl.bind("XF86AudioRaiseVolume", hl.dsp.exec_cmd("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+"), {
  repeating = true,
  description = "Raise volume",
})

hl.bind("XF86AudioLowerVolume", hl.dsp.exec_cmd("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"), {
  repeating = true,
  description = "Lower volume",
})

hl.bind("XF86AudioMute", hl.dsp.exec_cmd("wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"), {
  locked = true,
  description = "Mute volume",
})

-- Requires playerctl.
hl.bind("XF86AudioPlay", hl.dsp.exec_cmd("playerctl play-pause"), {
  locked = true,
  description = "Play/pause media",
})

hl.bind("XF86AudioPrev", hl.dsp.exec_cmd("playerctl previous"), {
  locked = true,
  description = "Previous media track",
})

hl.bind("XF86AudioNext", hl.dsp.exec_cmd("playerctl next"), {
  locked = true,
  description = "Next media track",
})

hl.bind("XF86MonBrightnessDown", hl.dsp.exec_cmd("brightnessctl s 5%-"), {
  description = "Lower display brightness",
})

hl.bind("XF86MonBrightnessUp", hl.dsp.exec_cmd("brightnessctl s 5%+"), {
  description = "Raise display brightness",
})

hl.bind("SHIFT + XF86MonBrightnessDown", hl.dsp.exec_cmd("brightnessctl -d '*::kbd_backlight' s 5%-"), {
  description = "Lower keyboard backlight",
})

hl.bind("SHIFT + XF86MonBrightnessUp", hl.dsp.exec_cmd("brightnessctl -d '*::kbd_backlight' s 5%+"), {
  description = "Raise keyboard backlight",
})