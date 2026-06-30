local function load_matugen_theme()
	local fallback = {
		active_border = "rgba(89b4faff)",
		inactive_border = "rgba(313244aa)",
		group_active = "rgba(cba6f7ff)",
		group_inactive = "rgba(313244aa)",
		locked_active = "rgba(f38ba8ff)",
		locked_inactive = "rgba(45475aaa)",
		background = "rgba(11111bff)",
	}

	local home = os.getenv("HOME")
	if not home then
		return fallback
	end

	local chunk = loadfile(home .. "/.cache/matugen/hypr-colors.lua")
	if not chunk then
		return fallback
	end

	local ok, generated_theme = pcall(chunk)
	if not ok or type(generated_theme) ~= "table" then
		return fallback
	end

	return setmetatable(generated_theme, { __index = fallback })
end

local theme = load_matugen_theme()

hl.config({
	input = {
		kb_layout = "us",
		kb_options = "ctrl:nocaps",

		follow_mouse = 1,
		mouse_refocus = true,
		sensitivity = 0,

		touchpad = {
			natural_scroll = true,
			disable_while_typing = true,
			tap_to_click = true,
			scroll_factor = 0.9,
		},
	},

	general = {
		gaps_in = 6,
		gaps_out = 18,
		border_size = 2,

		layout = "dwindle",

		resize_on_border = true,
		extend_border_grab_area = 12,
		hover_icon_on_border = true,

		col = {
			active_border = theme.active_border,
			inactive_border = theme.inactive_border,
		},
	},

	decoration = {
		rounding = 14,
		rounding_power = 2.4,

		active_opacity = 0.96,
		inactive_opacity = 0.88,
		fullscreen_opacity = 1.0,

		dim_inactive = true,
		dim_strength = 0.12,
		dim_special = 0.18,

		blur = {
			enabled = true,
			size = 16,
			passes = 5,
			new_optimizations = true,
			xray = false,
			ignore_opacity = true,
			vibrancy = 0.40,
			vibrancy_darkness = 0.25,
			noise = 0.012,
			contrast = 0.9,
			brightness = 0.82,
		},

		shadow = {
			enabled = true,
			range = 28,
			render_power = 4,
			sharp = false,
			scale = 1.0,
			color = "rgba(00000066)",
			color_inactive = "rgba(00000033)",
			offset = "0 6",
		},
	},

	animations = {
		enabled = true,

		bezier = {
			"wind, 0.05, 0.9, 0.1, 1.05",
			"winIn, 0.1, 1.1, 0.1, 1.0",
			"winOut, 0.3, -0.3, 0, 1",
			"liner, 1, 1, 1, 1",
			"overshot, 0.05, 0.9, 0.1, 1.08",
			"smoothOut, 0.36, 0, 0.66, -0.56",
			"smoothIn, 0.25, 1, 0.5, 1",
		},

		animation = {
			"windows, 1, 5, wind, slide",
			"windowsIn, 1, 5, winIn, slide",
			"windowsOut, 1, 4, smoothOut, slide",
			"windowsMove, 1, 5, wind, slide",
			"border, 1, 8, default",
			"borderangle, 1, 10, liner, loop",
			"fade, 1, 6, smoothIn",
			"fadeDim, 1, 5, smoothIn",
			"workspaces, 1, 5, overshot, slide",
			"specialWorkspace, 1, 5, overshot, slidevert",
		},
	},

	dwindle = {
		preserve_split = true,
		smart_split = false,
		smart_resizing = true,
		permanent_direction_override = false,
		force_split = 0,
		split_width_multiplier = 1.0,
		use_active_for_splits = true,
		default_split_ratio = 1.0,
	},

	master = {
		new_status = "master",
		new_on_top = true,
		mfact = 0.55,
		orientation = "left",
		smart_resizing = true,
		drop_at_cursor = true,
	},

	group = {
		auto_group = false,
		insert_after_current = true,
		focus_removed_window = true,

		groupbar = {
			enabled = true,
			font_size = 11,
			gradients = true,
			height = 26,
			priority = 3,
			render_titles = true,
			scrolling = true,

			col = {
				active = theme.active_border,
				inactive = theme.inactive_border,
				locked_active = theme.locked_active,
				locked_inactive = theme.locked_inactive,
			},
		},

		col = {
			border_active = theme.group_active,
			border_inactive = theme.group_inactive,
			border_locked_active = theme.locked_active,
			border_locked_inactive = theme.locked_inactive,
		},
	},

	misc = {
		disable_hyprland_logo = true,
		disable_splash_rendering = true,

		vrr = 1,

		animate_manual_resizes = true,
		animate_mouse_windowdragging = true,

		focus_on_activate = true,
		mouse_move_enables_dpms = true,
		key_press_enables_dpms = true,

		enable_swallow = false,
		swallow_regex = "^(kitty|foot|alacritty|wezterm)$",

		background_color = theme.background,

		exit_window_retains_fullscreen = true,

		initial_workspace_tracking = 1,
		middle_click_paste = false,
	},

	binds = {
		workspace_back_and_forth = true,
		allow_workspace_cycles = true,
		pass_mouse_when_bound = false,
		scroll_event_delay = 250,
	},

	gestures = {
		workspace_swipe_distance = 360,
		workspace_swipe_invert = true,
		workspace_swipe_min_speed_to_force = 30,
		workspace_swipe_cancel_ratio = 0.45,
		workspace_swipe_create_new = true,
		workspace_swipe_forever = false,
	},

	cursor = {
		no_hardware_cursors = false,
		use_cpu_buffer = false,
		sync_gsettings_theme = true,
		hide_on_key_press = true,
		hide_on_touch = true,
		inactive_timeout = 6,
		warp_on_change_workspace = 2,
	},

	xwayland = {
		force_zero_scaling = true,
		use_nearest_neighbor = false,
	},

	render = {
		direct_scanout = 1,
		cm_enabled = true,
		cm_auto_hdr = 0,
	},

	ecosystem = {
		no_update_news = true,
		no_donation_nag = true,
	},

	debug = {
		disable_logs = false,
	},
})
