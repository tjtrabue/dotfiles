-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

-- Pull in utility service providers
local lua_path_util = require("my-awm.util.lua-path-util")

-- The numeric version of Lua currently running (like '5.3').
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
lua_path_util.add_luarocks_paths(my_ver)

local os_util = require("my-awm.util.os-util")
-- Add extra executable paths to $PATH
local user_home_dir = os.getenv("HOME")
os_util.add_to_path(user_home_dir .. "/bin", user_home_dir .. "/.local/bin")

-- Standard awesome library
local awful = require("awful")
require("awful.autofocus")

-- Create the menu bar
local menubar = require("menubar")

-- Import local components
require("my-awm.error")
local default_progs = require("my-awm.default-programs")
local mouse_buttons = require("my-awm.mouse.mouse-buttons")
local autorun = require("my-awm.autorun")
local theme_manager = require("my-awm.theme.theme-manager")

-- Get the custom theme name from the theme file and set it.
local theme_name = theme_manager.get_theme_name_from_file()
theme_manager.set_custom_theme(theme_name)

-- Load file containing all layouts defined for Awesome session
require("my-awm.layout")

-- Menubar configuration
-- Set the terminal for applications that require it
menubar.utils.terminal = default_progs.terminal

--  Initialize screens (wibar, tasklist, wallpaper, etc.)
local screen_util = require("my-awm.screen")
awful.screen.connect_for_each_screen(screen_util.connect_screen)

-- Mouse bindings
root.buttons(mouse_buttons)

-- Retrieve global keybindings table
local global_keys = require("my-awm.taglist.taglist-keys")

-- Set global keybindings
root.keys(global_keys)

-- Apply rules to new clients as they appear
require("my-awm.client.client-rules")

-- Signals
require("my-awm.client.client-signals")

-- List of apps to run on startup.
local apps_to_autorun = {
  -- Terminals
  {
    name = default_progs.terminal,
    opts = "",
    tag = "main"
  },
  {
    name = default_progs.terminal,
    opts = "",
    tag = "main"
  },
  {
    name = default_progs.terminal,
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
    name = default_progs.browser,
    opts = "",
    tag = "www"
  }
}

-- Automatically start programs when Awesome kicks off.
autorun(apps_to_autorun)
