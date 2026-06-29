local binds = {
	{
		from = "CONTROL + n",
		to = {
			mods = "",
			key = "down",
		},
		table = { repeating = true },
	},
	{
		from = "CONTROL + p",
		to = {
			mods = "",
			key = "up",
		},
		table = { repeating = true },
	},
	{
		from = "CONTROL + s",
		to = {
			mods = "CONTROL",
			key = "f",
		},
		table = {},
	},
	{
		from = "CONTROL + a",
		to = {
			mods = "",
			key = "home",
		},
		table = {},
	},
	{
		from = "CONTROL + e",
		to = {
			mods = "",
			key = "end",
		},
		table = {},
	},
	{
		from = "ALT + b",
		to = {
			mods = "CONTROL",
			key = "left",
		},
		table = { repeating = true },
	},
	{
		from = "ALT + f",
		to = {
			mods = "CONTROL",
			key = "right",
		},
		table = { repeating = true },
	},
}
local function firefox_only_remap(from_keys, shortcut, bind_table)
	hl.bind(from_keys, function()
		local w = hl.get_active_window()
		if w ~= nil and w.class == "Firefox" then
			hl.dispatch(
				hl.dsp.send_shortcut({
					mods = shortcut.mods,
					key = shortcut.key,
					window = "activewindow",
				})
			)
		else
			-- Preserve the real shortcut in every non-Firefox app.
			hl.dispatch(hl.dsp.pass({ window = "activewindow" }))
		end
	end, bind_table)
end

for _, bind in ipairs(binds) do
	firefox_only_remap(bind.from, bind.to, bind.table)
end
