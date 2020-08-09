pcall(require, "luarocks.loader")

local awful = require("awful")
local beautiful = require("beautiful")
local awesome_menu = require("my-awm.awesome-menu")
local vars = require("my-awm.vars")
local default_progs = require("my-awm.default-programs")
local theme_menu = require("my-awm.theme.theme-menu")

-- The menu in the top left hand corner of the screen.
local main_menu =
    awful.menu(
    {
        items = {
            {"awesome", awesome_menu, beautiful.awesome_icon},
            {"open terminal", default_progs.terminal}
        }
    }
)

return main_menu
