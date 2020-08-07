local awful = require("awful")

-- Contains important file paths used by Awesome widgets
local paths = {}

paths.config_dir = awful.util.getdir("config")
paths.icons_dir = "/usr/share/icons"
paths.home_dir = os.getenv("HOME")
paths.luarocks_user_dir = paths.home_dir .. "/.luarocks"
paths.system_dir = "/usr/share/awesome"

return paths
