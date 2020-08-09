local awful = require("awful")
local gfs = require("gears.filesystem")

-- Contains important file paths used by Awesome widgets
local paths = {}

-- Main Awesome WM directory for the entire computer
paths.system_dir = "/usr/share/awesome"
-- User's Awesome WM configuration directory
paths.config_dir = gfs.get_configuration_dir()
-- Icons directories
paths.icon_dir = "/usr/share/icons"
paths.awesome_icon_dir = gfs.get_awesome_icon_dir()
paths.home_dir = os.getenv("HOME")
-- User installed LuaRocks packages directory
paths.luarocks_user_dir = paths.home_dir .. "/.luarocks"
-- Awesome theme directory paths (theme = color scheme in the Awesome world)
paths.themes_system_dir = gfs.get_themes_dir()
paths.themes_custom_dir = paths.config_dir .. "/themes"
-- This file contains a single line, which is the name of the theme to use
paths.themes_custom_file = paths.themes_custom_dir .. "/theme"

return paths
