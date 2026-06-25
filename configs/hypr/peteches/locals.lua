local locals = {}

locals.term_cmd = "emacsclient --create-frame --eval '(ghostel)'"
locals.editor_cmd = "emacsclient --create-frame"
locals.screenshot_cmd = "wofi-screenshot.sh"
locals.browser_cmd = "firefox-profile-launcher"
locals.password_lookup_cmd = "wofi-password.sh"
locals.ai_switch_cmd = "ai-switcher.sh"
locals.random_wallpaper_cmd = "wofi-dms-random-wallpaper"


local p = io.popen("hostname")
local host = p:read("*l")
p:close()

locals.host = host

return locals
