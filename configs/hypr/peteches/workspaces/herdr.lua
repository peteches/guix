-- special:herdr uses scrolling, same as music/messaging, so the three
-- per-account herdr panes sit side by side and you scroll between them.
hl.workspace_rule({
  workspace = "special:herdr",
  layout = "scrolling",
  persistent = true,
})

-- all +herdr tagged windows go to the herdr workspace.
hl.window_rule({
    match = {
	tag = "herdr"
    },
    workspace = "special:herdr silent",
})
