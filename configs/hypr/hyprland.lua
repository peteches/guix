require("peteches.env")
require("peteches.autostart")
require("peteches.keybinds")
require("peteches.variables")

local locals = require("peteches.locals")

-- host specific configs
local module_name = "peteches.systems." .. locals.host

require(module_name)