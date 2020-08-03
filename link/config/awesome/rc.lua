-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
local menubar = require("menubar")

-- Import local components
require("my-awm.error")
local my_vars = require("my-awm.vars")
local my_helpers = require("my-awm.helper-fns")
local wp_util = require("my-awm.wallpaper")
local mouse_buttons = require("my-awm.mouse.mouse-buttons")
local autorun = require("my-awm.autorun")

-- Load additional features
-- For a better Modkey+Tab cycling experience (must be cloned)
local cyclefocus = require("awesome-cyclefocus")

-- Load file containing all layouts defined for Awesome session
require("my-awm.layout")

-- Menu
-- Menu for the Awesome button and right-click context
local main_menu = require("my-awm.main-menu")

-- Menubar configuration
-- Set the terminal for applications that require it
menubar.utils.terminal = my_vars.terminal

--  Initialize screens (wibar, tasklist, wallpaper, etc.)
local screen_util = require("my-awm.screen")
awful.screen.connect_for_each_screen(screen_util.connect_screen)

-- Mouse bindings
root.buttons(mouse_buttons)

-- Key bindings
local globalkeys = require("my-awm.global.global-keys")

require("my-awm.taglist.taglist-keys")

-- Set keys
root.keys(globalkeys)

-- Apply rules to new clients as they appear
require("my-awm.client.client-rules")

-- Signals
require("my-awm.client.client-signals")

-- List of apps to run on startup.
local apps_to_autorun = {
    -- Terminals
    {
        name = my_vars.terminal,
        opts = "",
        tag = "main"
    },
    {
        name = my_vars.terminal,
        opts = "",
        tag = "main"
    },
    {
        name = my_vars.terminal,
        opts = "",
        tag = "main"
    },
    -- Emacs
    {
        name = "emacs",
        opts = "",
        tag = "edit"
    },
    -- Web browser
    {
        name = my_vars.browser,
        opts = "",
        tag = "www"
    }
}

-- Automatically start programs when Awesome kicks off.
autorun(apps_to_autorun)
