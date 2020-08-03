local awful = require("awful")
local gears = require("gears")
local hotkeys_popup = require("awful.hotkeys_popup")
-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require("awful.hotkeys_popup.keys")
local volume_widget = require("awesome-wm-widgets.volume-widget.volume")

local my_vars = require("my-awm.vars")
local main_menu = require("my-awm.main-menu")

-- Global Awesome WM keybindings
local global_keys =
    gears.table.join(
    awful.key({my_vars.modkey}, "s", hotkeys_popup.show_help, {description = "show help", group = "awesome"}),
    awful.key({my_vars.modkey}, "Left", awful.tag.viewprev, {description = "view previous tag", group = "tag"}),
    awful.key({my_vars.modkey}, "u", awful.tag.viewprev, {description = "view previous tag", group = "tag"}),
    awful.key({my_vars.modkey}, "Right", awful.tag.viewnext, {description = "view next tag", group = "tag"}),
    awful.key({my_vars.modkey}, "i", awful.tag.viewnext, {description = "view next tag", group = "tag"}),
    awful.key({my_vars.modkey}, "Escape", awful.tag.history.restore, {description = "go back", group = "tag"}),
    -- Open a window switcher
    awful.key(
        {my_vars.modkey},
        "y",
        function()
            awful.spawn("rofi -show window")
        end,
        {description = "Open a window switcher", group = "client"}
    ),
    awful.key(
        {my_vars.modkey},
        "j",
        function()
            awful.client.focus.byidx(1)
        end,
        {description = "focus next by index", group = "client"}
    ),
    awful.key(
        {my_vars.modkey},
        "k",
        function()
            awful.client.focus.byidx(-1)
        end,
        {description = "focus previous by index", group = "client"}
    ),
    awful.key(
        {my_vars.modkey},
        "w",
        function()
            main_menu:show()
        end,
        {description = "show main menu", group = "awesome"}
    ),
    -- Layout manipulation
    awful.key(
        {my_vars.modkey, "Shift"},
        "j",
        function()
            awful.client.swap.byidx(1)
        end,
        {description = "swap with next client by index", group = "client"}
    ),
    awful.key(
        {my_vars.modkey, "Shift"},
        "k",
        function()
            awful.client.swap.byidx(-1)
        end,
        {description = "swap with previous client by index", group = "client"}
    ),
    awful.key(
        {my_vars.modkey, "Control"},
        "j",
        function()
            awful.screen.focus_relative(1)
        end,
        {description = "focus the next screen", group = "screen"}
    ),
    awful.key(
        {my_vars.modkey, "Control"},
        "k",
        function()
            awful.screen.focus_relative(-1)
        end,
        {description = "focus the previous screen", group = "screen"}
    ),
    awful.key(
        {my_vars.modkey, "Shift"},
        "u",
        awful.client.urgent.jumpto,
        {description = "jump to urgent client", group = "client"}
    ),
    -- modkey+Tab: cycle through all clients.
    awful.key(
        {my_vars.modkey},
        "Tab",
        function(c)
            cyclefocus.cycle({modifier = "Super_L"})
        end
    ),
    -- modkey+Shift+Tab: backwards
    awful.key(
        {my_vars.modkey, "Shift"},
        "Tab",
        function(c)
            cyclefocus.cycle({modifier = "Super_L"})
        end
    ),
    -- Standard program
    awful.key(
        {my_vars.modkey},
        "Return",
        function()
            awful.spawn(my_vars.terminal)
        end,
        {description = "open a terminal", group = "launcher"}
    ),
    awful.key(
        {my_vars.modkey},
        "e",
        function()
            awful.spawn("emacs")
        end,
        {description = "open emacs", group = "launcher"}
    ),
    awful.key(
        {my_vars.modkey, "Shift"},
        "f",
        function()
            awful.spawn(my_vars.browser)
        end,
        {description = "open " .. my_vars.browser, group = "launcher"}
    ),
    awful.key({my_vars.modkey, "Control"}, "r", awesome.restart, {description = "reload awesome", group = "awesome"}),
    awful.key({my_vars.modkey, "Shift"}, "q", awesome.quit, {description = "quit awesome", group = "awesome"}),
    awful.key(
        {my_vars.modkey},
        "l",
        function()
            awful.tag.incmwfact(0.05)
        end,
        {description = "increase master width factor", group = "layout"}
    ),
    awful.key(
        {my_vars.modkey},
        "h",
        function()
            awful.tag.incmwfact(-0.05)
        end,
        {description = "decrease master width factor", group = "layout"}
    ),
    awful.key(
        {my_vars.modkey, "Shift"},
        "h",
        function()
            awful.tag.incnmaster(1, nil, true)
        end,
        {description = "increase the number of master clients", group = "layout"}
    ),
    awful.key(
        {my_vars.modkey, "Shift"},
        "l",
        function()
            awful.tag.incnmaster(-1, nil, true)
        end,
        {description = "decrease the number of master clients", group = "layout"}
    ),
    awful.key(
        {my_vars.modkey, "Control"},
        "h",
        function()
            awful.tag.incncol(1, nil, true)
        end,
        {description = "increase the number of columns", group = "layout"}
    ),
    awful.key(
        {my_vars.modkey, "Control"},
        "l",
        function()
            awful.tag.incncol(-1, nil, true)
        end,
        {description = "decrease the number of columns", group = "layout"}
    ),
    awful.key(
        {my_vars.modkey},
        "space",
        function()
            awful.layout.inc(1)
        end,
        {description = "select next", group = "layout"}
    ),
    awful.key(
        {my_vars.modkey, "Shift"},
        "space",
        function()
            awful.layout.inc(-1)
        end,
        {description = "select previous", group = "layout"}
    ),
    awful.key(
        {my_vars.modkey, "Control"},
        "n",
        function()
            local c = awful.client.restore()
            -- Focus restored client
            if c then
                c:emit_signal("request::activate", "key.unminimize", {raise = true})
            end
        end,
        {description = "restore minimized", group = "client"}
    ),
    -- Prompt
    awful.key(
        {my_vars.modkey},
        "r",
        function()
            awful.spawn("rofi -show run")
        end,
        {description = "run a generic command", group = "launcher"}
    ),
    awful.key(
        {my_vars.modkey},
        "x",
        function()
            awful.prompt.run {
                prompt = "Run Lua code: ",
                textbox = awful.screen.focused().mypromptbox.widget,
                exe_callback = awful.util.eval,
                history_path = awful.util.get_cache_dir() .. "/history_eval"
            }
        end,
        {description = "lua execute prompt", group = "awesome"}
    ),
    -- Menubar
    awful.key(
        {my_vars.modkey},
        "p",
        function()
            awful.spawn("rofi -show drun")
        end,
        {description = "launch an application", group = "launcher"}
    ),
    awful.key(
        {my_vars.modkey, "Shift"},
        "s",
        function()
            awful.spawn("rofi -show ssh")
        end,
        {description = "Connect to remote server via SSH", group = "launcher"}
    ),
    -- Backlight/brightness
    awful.key(
        {},
        "XF86MonBrightnessUp",
        function()
            awful.spawn("xbacklight -inc 3")
        end,
        {description = "Increase brightness", group = "awesome"}
    ),
    awful.key(
        {},
        "XF86MonBrightnessDown",
        function()
            awful.spawn("xbacklight -dec 3")
        end,
        {description = "Decrease brightness", group = "awesome"}
    ),
    -- Audio controls
    awful.key(
        {},
        "XF86AudioRaiseVolume",
        -- function()
        --   awful.spawn("amixer -q sset Master,0 1+ unmute")
        -- end,
        volume_widget.raise,
        {description = "Raise volume", group = "awesome"}
    ),
    awful.key(
        {},
        "XF86AudioLowerVolume",
        -- function()
        --   awful.spawn("amixer -q sset Master,0 1- unmute")
        -- end,
        volume_widget.lower,
        {description = "Lower volume", group = "awesome"}
    ),
    awful.key(
        {},
        "XF86AudioMute",
        -- function()
        --   awful.spawn("amixer -q sset Master,0 toggle")
        -- end,
        volume_widget.toggle,
        {description = "Toggle mute", group = "awesome"}
    ),
    -- Sleep, hibernate, lock, etc.
    awful.key(
        {my_vars.modkey, "Shift", "Control"},
        "l",
        function()
            awful.spawn("xscreensaver-command -lock")
        end,
        {description = "Lock the display", group = "system"}
    ),
    awful.key(
        {my_vars.modkey, "Shift", "Control"},
        "s",
        function()
            awful.spawn("systemctl suspend")
        end,
        {description = "Put computer to sleep", group = "system"}
    )
)

return global_keys
