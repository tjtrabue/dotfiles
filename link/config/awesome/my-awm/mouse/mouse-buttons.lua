local awful = require("awful")
local gears = require("gears")

local main_menu = require("my-awm.main-menu")

local mouse_buttons =
    gears.table.join(
    awful.button(
        {},
        3,
        function()
            main_menu:toggle()
        end
    ),
    awful.button({}, 4, awful.tag.viewnext),
    awful.button({}, 5, awful.tag.viewprev)
)

return mouse_buttons
