pcall(require, "luarocks.loader")

local awful = require("awful")
local beautiful = require("beautiful")
local awesome_menu = require("my-awm.awesome-menu")
local vars = require("my-awm.vars")

-- The menu in the top left hand corner of the screen.
local main_menu =
    awful.menu(
    {
        items = {
            {"awesome", awesome_menu, beautiful.awesome_icon},
            {"open terminal", vars.terminal}
        }
    }
)

return main_menu
