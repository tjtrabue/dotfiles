local awful = require("awful")

-- Contains important file paths used by Awesome widgets
local paths = {}

-- Main Awesome WM directory for the entire computer
paths.system_dir = "/usr/share/awesome"
-- User's Awesome WM configuration directory
paths.config_dir = awful.util.getdir("config")
paths.icons_dir = "/usr/share/icons"
paths.home_dir = os.getenv("HOME")
-- User installed LuaRocks packages directory
paths.luarocks_user_dir = paths.home_dir .. "/.luarocks"
-- Awesome theme directory paths (theme = color scheme in the Awesome world)
paths.themes_system_dir = paths.system_dir .. "/themes"
paths.themes_custom_dir = paths.config_dir .. "/themes"

return paths
