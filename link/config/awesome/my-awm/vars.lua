-- Variables to use throughout Awesome

local beautiful = require("beautiful")
local gears = require("gears")

-- Local imports
local my_helpers = require("my-awm.helper-fns")

local vars = {}

-- Themes define colours, icons, font and wallpapers.
vars.theme = "/home/" .. os.getenv("USER") .. "/.config/awesome/theme.lua"
if not my_helpers.file_exists(vars.theme) then
    -- Fall back to default theme if user theme does not exist
    theme = gears.filesystem.get_themes_dir() .. "default/theme.lua"
end
beautiful.init(vars.theme)

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
vars.modkey = "Mod4"

-- Names for the workspaces
vars.tag_names = {
    "main",
    "edit",
    "www",
    "log",
    "email",
    "im",
    "gimp",
    "office",
    "misc"
}

vars.num_tags = #vars.tag_names

return vars
