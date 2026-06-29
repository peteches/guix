-- special:messaging uses scrolling
hl.workspace_rule({
  workspace = "special:messaging",
  layout = "scrolling",
  persistent = true,
})

-- all +messaging tagged windows go to the messaging workspace.
hl.window_rule({
    match = {
	tag = "messaging"
    },
    workspace = "special:messaging silent",
})


