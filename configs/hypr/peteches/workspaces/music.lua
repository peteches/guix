-- special:music uses scrolling
hl.workspace_rule({
  workspace = "special:music",
  layout = "scrolling",
  persistent = true,
})

-- all +music tagged windows go to the music workspace.
hl.window_rule({
    match = {
	tag = "music"
    },
    workspace = "special:music silent",
})
