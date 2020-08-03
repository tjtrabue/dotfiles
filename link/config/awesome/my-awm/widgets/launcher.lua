local awful = require("awful")
local beautiful = require("beautiful")

local main_menu = require("my-awm.main-menu")

-- The button in the top left hand corner of the screen (the Awesome button).
local launcher =
    awful.widget.launcher(
    {
        image = beautiful.awesome_icon,
        menu = main_menu
    }
)

return launcher
