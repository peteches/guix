require("peteches.env")
require("peteches.autostart")
require("peteches.keybinds")
require("peteches.variables")
require("peteches.layer-rules")
require("peteches.animations")
require("peteches.workspaces")
require("peteches.apps")

hl.notification.create({ text = "starting hyprland", timeout = 30 })

local locals = require("peteches.locals")

-- host specific configs
local module_name = "peteches.systems." .. locals.host

require(module_name)
