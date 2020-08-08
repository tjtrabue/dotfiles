pcall(require, "luarocks.loader")

local awful = require("awful")
local my_vars = require("my-awm.vars")
local default_progs = require("my-awm.default-programs")
require("awful.hotkeys_popup.keys")
local hotkeys_popup = require("awful.hotkeys_popup")

-- Create a launcher widget and a main menu
local my_awesome_menu = {
    {
        "hotkeys",
        function()
            hotkeys_popup.show_help(nil, awful.screen.focused())
        end
    },
    {"manual", default_progs.terminal .. " -e man awesome"},
    {"edit config", default_progs.editor_cmd .. " " .. awesome.conffile},
    {"restart", awesome.restart},
    {
        "quit",
        function()
            awesome.quit()
        end
    }
}

return my_awesome_menu
