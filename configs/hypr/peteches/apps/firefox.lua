local binds = {
	ctrln = hl.bind("CONTROL + n", hl.dsp.send_shortcut({
		mods = "",
		key = "down",
		window = "activewindow",
	})),
	ctrlp = hl.bind("CONTROL + p", hl.dsp.send_shortcut({
		mods = "",
		key = "up",
		window = "activewindow",
	})),
	ctrls = hl.bind("CONTROL + s", hl.dsp.send_shortcut({
		mods = "CONTROL",
		key = "f",
		window = "activewindow",
	})),
	ctrla = hl.bind("CONTROL + a", hl.dsp.send_shortcut({
		mods = "",
		key = "home",
		window = "activewindow",
	})),
	ctrle = hl.bind("CONTROL + e", hl.dsp.send_shortcut({
		mods = "",
		key = "end",
		window = "activewindow",
	})),
	altb = hl.bind("ALT + b", hl.dsp.send_shortcut({
		mods = "CONTROL",
		key = "left",
		window = "activewindow",
	})),
	altf = hl.bind("ALT + f", hl.dsp.send_shortcut({
		mods = "CONTROL",
		key = "right",
		window = "activewindow",
	})),

}

local set_bind_enabled_state = function(state)
	for _, bind in ipairs(binds) do
		bind:set_enabled(state)
	end
end

set_bind_enabled_state(false)

hl.on("window.active",
	function(w)
		if w.class == "Firefox" then
			set_bind_enabled_state(true)
		end
	end)
