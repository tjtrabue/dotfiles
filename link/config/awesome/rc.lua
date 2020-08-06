-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

-- Pull in helper functions
local helpers = require("my-awm.helper-fns")

-- The version of Lua currently running.
local my_ver = string.sub(_VERSION, string.find(_VERSION, "%d.%d"))

-- REMOVE THIS ONCE AWESOME UPDATES TO LUA 5.4!
-- Test before you remove it, of course.
--[[
  Right now this statement is necessary for Awesome to find our user-installed
  LuaRocks for Lua 5.3. The reason for this is that once our system-wide Lua
  version got bumped up to 5.4 it also updated luarocks to be a Lua 5.4
  package. Thus, the 'pcall(require, "luarocks.loader")' line above this only
  works for Lua 5.4 luarocks packages. However, Awesome WM is still a Lua 5.3
  application and therefore will not see our system-wide luarocks installation
  at all, meaning that we have to manually set the luarocks path here.
--]]
helpers.add_luarocks_paths(my_ver)

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
