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
local hotkeys_popup = require("awful.hotkeys_popup")
-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require("awful.hotkeys_popup.keys")

-- Load additional widgets from awesome-wm-widgets repo (must be cloned)
local battery_widget = require("awesome-wm-widgets.batteryarc-widget.batteryarc")
local volume_widget = require("awesome-wm-widgets.volume-widget.volume")
local ram_widget = require("awesome-wm-widgets.ram-widget.ram-widget")
local cpu_widget = require("awesome-wm-widgets.cpu-widget.cpu-widget")
local brightness_widget = require("awesome-wm-widgets.brightness-widget.brightness")
local weather_widget = require("awesome-wm-widgets.weather-widget.weather")

require("my-awm-components.error")

-- Load additional features
-- For a better Modkey+Tab cycling experience (must be cloned)
local cyclefocus = require("awesome-cyclefocus")

-- {{{ Helper functions
local function file_exists(name)
    local f = io.open(name, "r")
    if f ~= nil then
        io.close(f)
        return true
    else
        return false
    end
end
-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.
local theme = "/home/" .. os.getenv("USER") .. "/.config/awesome/theme.lua"
if not file_exists(theme) then
    -- Fall back to default theme if user theme does not exist
    theme = gears.filesystem.get_themes_dir() .. "default/theme.lua"
end
beautiful.init(theme)

-- This is used later as the default terminal and editor to run.
local terminal = "alacritty"
local browser = "firefox"
local editor = os.getenv("EDITOR") or "vim"
local editor_cmd = terminal .. " -e " .. editor

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
local modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
    awful.layout.suit.tile,
    -- awful.layout.suit.tile.left,
    -- awful.layout.suit.tile.bottom,
    -- awful.layout.suit.tile.top,
    awful.layout.suit.fair,
    -- awful.layout.suit.fair.horizontal,
    -- awful.layout.suit.spiral,
    awful.layout.suit.spiral.dwindle,
    awful.layout.suit.max,
    -- awful.layout.suit.max.fullscreen,
    awful.layout.suit.magnifier,
    -- awful.layout.suit.corner.nw,
    -- awful.layout.suit.corner.ne,
    -- awful.layout.suit.corner.sw,
    -- awful.layout.suit.corner.se,
    awful.layout.suit.floating
}
-- }}}

-- {{{ Menu
-- Create a launcher widget and a main menu
local myawesomemenu = {
    {
        "hotkeys",
        function()
            hotkeys_popup.show_help(nil, awful.screen.focused())
        end
    },
    {"manual", terminal .. " -e man awesome"},
    {"edit config", editor_cmd .. " " .. awesome.conffile},
    {"restart", awesome.restart},
    {
        "quit",
        function()
            awesome.quit()
        end
    }
}

local mymainmenu =
    awful.menu(
    {
        items = {
            {"awesome", myawesomemenu, beautiful.awesome_icon},
            {"open terminal", terminal}
        }
    }
)

local mylauncher =
    awful.widget.launcher(
    {
        image = beautiful.awesome_icon,
        menu = mymainmenu
    }
)

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

-- Keyboard map indicator and switcher
-- Not currently used because all it does is display your layout name
-- local mykeyboardlayout = awful.widget.keyboardlayout()

-- {{{ Wibar
-- Create a textclock widget
local mytextclock = wibox.widget.textclock()

-- Create a wibox for each screen and add it
local taglist_buttons =
    gears.table.join(
    awful.button(
        {},
        1,
        function(t)
            t:view_only()
        end
    ),
    awful.button(
        {modkey},
        1,
        function(t)
            if client.focus then
                client.focus:move_to_tag(t)
            end
        end
    ),
    awful.button({}, 3, awful.tag.viewtoggle),
    awful.button(
        {modkey},
        3,
        function(t)
            if client.focus then
                client.focus:toggle_tag(t)
            end
        end
    ),
    awful.button(
        {},
        4,
        function(t)
            awful.tag.viewnext(t.screen)
        end
    ),
    awful.button(
        {},
        5,
        function(t)
            awful.tag.viewprev(t.screen)
        end
    )
)

local tasklist_buttons =
    gears.table.join(
    awful.button(
        {},
        1,
        function(c)
            if c == client.focus then
                c.minimized = true
            else
                c:emit_signal("request::activate", "tasklist", {raise = true})
            end
        end
    ),
    awful.button(
        {},
        3,
        function()
            awful.menu.client_list({theme = {width = 250}})
        end
    ),
    awful.button(
        {},
        4,
        function()
            awful.client.focus.byidx(1)
        end
    ),
    awful.button(
        {},
        5,
        function()
            awful.client.focus.byidx(-1)
        end
    )
)
-- }}}

-- Tag (workspace) information {{{
-- Each screen has its own tag table.
local tag_names = {
    "main",
    "edit",
    "www",
    "log",
    "email",
    "im",
    "gimp",
    "office",
    "misc"
}
-- }}}

-- Wallpaper {{{
-- Set according to wallpaper directory
local wallpaper_dir_path = os.getenv("HOME") .. "/.dotfiles/img/wallpaper/"
-- Set to number of used tags
local num_tags = #tag_names
-- Other variables
local num_files = 0
local wp_all = {}
local wp_selected = {}

math.randomseed(os.time())
-- To guarantee unique random numbers on every platform, pop a few
for i = 1, 10 do
    math.random()
end

--- LUA implementation of PHP scan dir
--- Returns all files (except . and ..) in "directory"
-- @param directory The directory to scan for files
local function scandir(directory)
    num_files, t, popen = 0, {}, io.popen
    for filename in popen('ls -a "' .. directory .. '"'):lines() do
        -- If case to disregard "." and ".."
        if (not (filename == "." or filename == "..")) then
            num_files = num_files + 1
            t[num_files] = filename
        end
    end
    return t
end

--- Basically a modern Fisher-Yates shuffle
--- Returns "tabs" elements from an table "wp" of length "files"
--- Guarantees no duplicated elements in the return while having linear runtime
-- @param wp The wallpaper image file array
-- @param files The files array
-- @param tags The array of tags
local function select(wp, files, tags)
    local selected = {}
    for i = 1, tags do
        position = math.random(1, files)
        selected[i] = wp[position]
        wp[position] = wp[files]
        files = files - 1
    end
    return selected
end

-- Get the names of "num_tags" files from "num_files" total files in the
-- wallpaper directory path.
wp_selected = select(scandir(wallpaper_dir_path), num_files, num_tags)

--- Randomly sets wallpaper for each tag in a screen from a wallpaper directory.
-- @param scr Screen variable
local function set_wallpaper(scr)
    -- Set wallpaper on first tag (else it would be empty at start up)
    gears.wallpaper.fit(wallpaper_dir_path .. wp_selected[1], scr)
    -- Go over each tag
    for t = 1, num_tags do
        scr.tags[t]:connect_signal(
            "property::selected",
            function(tag)
                -- And if selected
                if not tag.selected then
                    return
                end
                -- Set wallpaper
                gears.wallpaper.fit(wallpaper_dir_path .. wp_selected[t], scr)
            end
        )
    end
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)
-- }}}

--  Initialize screens (wibar, tasklist, wallpaper, etc.) {{{
awful.screen.connect_for_each_screen(
    function(s)
        local l = awful.layout.suit
        local layouts = {}
        -- Fill in the layouts table with appropriate layouts.
        for i = 1, num_tags do
            layouts[i] = l.tile
        end
        awful.tag(tag_names, s, layouts)

        -- Create a promptbox for each screen
        s.mypromptbox = awful.widget.prompt()
        -- Create an imagebox widget which will contain an icon indicating which layout we're using.
        -- We need one layoutbox per screen.
        s.mylayoutbox = awful.widget.layoutbox(s)
        s.mylayoutbox:buttons(
            gears.table.join(
                awful.button(
                    {},
                    1,
                    function()
                        awful.layout.inc(1)
                    end
                ),
                awful.button(
                    {},
                    3,
                    function()
                        awful.layout.inc(-1)
                    end
                ),
                awful.button(
                    {},
                    4,
                    function()
                        awful.layout.inc(1)
                    end
                ),
                awful.button(
                    {},
                    5,
                    function()
                        awful.layout.inc(-1)
                    end
                )
            )
        )

        -- Create a powerline-inspired taglist widget
        s.mytaglist =
            awful.widget.taglist {
            screen = s,
            filter = awful.widget.taglist.filter.all,
            style = {
                shape = gears.shape.powerline
            },
            layout = {
                spacing = -12,
                spacing_widget = {
                    color = "#515151",
                    shape = gears.shape.powerline,
                    widget = wibox.widget.separator
                },
                layout = wibox.layout.fixed.horizontal
            },
            widget_template = {
                {
                    {
                        {
                            {
                                {
                                    id = "index_role",
                                    widget = wibox.widget.textbox
                                },
                                margins = 4,
                                widget = wibox.container.margin
                            },
                            bg = "#f99157",
                            fg = "#2d2d2d",
                            shape = gears.shape.circle,
                            widget = wibox.container.background
                        },
                        {
                            {
                                id = "icon_role",
                                widget = wibox.widget.imagebox
                            },
                            margins = 2,
                            widget = wibox.container.margin
                        },
                        {
                            id = "text_role",
                            widget = wibox.widget.textbox
                        },
                        layout = wibox.layout.fixed.horizontal
                    },
                    left = 18,
                    right = 18,
                    widget = wibox.container.margin
                },
                id = "background_role",
                widget = wibox.container.background,
                -- Add support for hover colors and an index label
                create_callback = function(self, c3, index, objects) --luacheck: no unused args
                    self:get_children_by_id("index_role")[1].markup = "<b> " .. index .. " </b>"
                    self:connect_signal(
                        "mouse::enter",
                        function()
                            local hover_color = "#99cc99"
                            if self.bg ~= hover_color then
                                self.backup_bg = self.bg
                                self.backup_fg = self.fg
                                self.has_backup = true
                            end
                            self.bg = hover_color
                            self.fg = "#2d2d2d"
                        end
                    )
                    self:connect_signal(
                        "mouse::leave",
                        function()
                            if self.has_backup then
                                self.bg = self.backup_bg
                                self.fg = self.backup_fg
                            end
                        end
                    )
                end,
                update_callback = function(self, c3, index, objects) --luacheck: no unused args
                    self:get_children_by_id("index_role")[1].markup = "<b> " .. index .. " </b>"
                end
            },
            buttons = taglist_buttons
        }

        -- Wallpaper
        set_wallpaper(s)

        -- Create a tasklist widget
        s.mytasklist =
            awful.widget.tasklist {
            screen = s,
            filter = awful.widget.tasklist.filter.currenttags,
            buttons = tasklist_buttons
        }

        -- Create the wibox
        s.mywibox = awful.wibar({position = "top", screen = s})

        -- Create a separator to space out the widgets
        local separator = wibox.widget.textbox()
        separator:set_text(" ♦ ")

        -- Add widgets to the wibox
        s.mywibox:setup {
            layout = wibox.layout.align.horizontal,
            {
                -- Left widgets
                layout = wibox.layout.fixed.horizontal,
                mylauncher,
                s.mytaglist,
                s.mypromptbox
            },
            s.mytasklist, -- Middle widget
            {
                -- Right widgets
                layout = wibox.layout.fixed.horizontal,
                -- mykeyboardlayout,
                wibox.widget.systray(),
                -- separator,
                cpu_widget(),
                separator,
                ram_widget(),
                separator,
                brightness_widget(
                    {
                        get_brightness_cmd = "xbacklight -get",
                        inc_brightness_cmd = "xbacklight -inc 3",
                        dec_brightness_cmd = "xbacklight -dec 3"
                    }
                ),
                separator,
                volume_widget({}),
                separator,
                battery_widget(),
                separator,
                weather_widget(
                    {
                        -- API key for https://home.openweathermap.org
                        -- You must create one if you don't already have one
                        -- registered.
                        api_key = "8e91475d8e1648e93792d9d1ecf27533",
                        -- TODO: Find programmatic way of calculating coordinates.
                        coordinates = {39.2909, -76.6108},
                        time_format_12h = true,
                        units = "imperial",
                        -- Do not show both Fahrenheit and Celsius
                        both_units_widget = false,
                        icons = "weather-underground-icons",
                        show_hourly_forecast = true,
                        show_daily_forecast = true
                    }
                ),
                separator,
                mytextclock,
                separator,
                s.mylayoutbox
            }
        }
    end
)
-- }}}

-- {{{ Mouse bindings
root.buttons(
    gears.table.join(
        awful.button(
            {},
            3,
            function()
                mymainmenu:toggle()
            end
        ),
        awful.button({}, 4, awful.tag.viewnext),
        awful.button({}, 5, awful.tag.viewprev)
    )
)
-- }}}

-- {{{ Key bindings
local globalkeys =
    gears.table.join(
    awful.key({modkey}, "s", hotkeys_popup.show_help, {description = "show help", group = "awesome"}),
    awful.key({modkey}, "Left", awful.tag.viewprev, {description = "view previous tag", group = "tag"}),
    awful.key({modkey}, "u", awful.tag.viewprev, {description = "view previous tag", group = "tag"}),
    awful.key({modkey}, "Right", awful.tag.viewnext, {description = "view next tag", group = "tag"}),
    awful.key({modkey}, "i", awful.tag.viewnext, {description = "view next tag", group = "tag"}),
    awful.key({modkey}, "Escape", awful.tag.history.restore, {description = "go back", group = "tag"}),
    -- Open a window switcher
    awful.key(
        {modkey},
        "y",
        function()
            awful.spawn("rofi -show window")
        end,
        {description = "Open a window switcher", group = "client"}
    ),
    awful.key(
        {modkey},
        "j",
        function()
            awful.client.focus.byidx(1)
        end,
        {description = "focus next by index", group = "client"}
    ),
    awful.key(
        {modkey},
        "k",
        function()
            awful.client.focus.byidx(-1)
        end,
        {description = "focus previous by index", group = "client"}
    ),
    awful.key(
        {modkey},
        "w",
        function()
            mymainmenu:show()
        end,
        {description = "show main menu", group = "awesome"}
    ),
    -- Layout manipulation
    awful.key(
        {modkey, "Shift"},
        "j",
        function()
            awful.client.swap.byidx(1)
        end,
        {description = "swap with next client by index", group = "client"}
    ),
    awful.key(
        {modkey, "Shift"},
        "k",
        function()
            awful.client.swap.byidx(-1)
        end,
        {description = "swap with previous client by index", group = "client"}
    ),
    awful.key(
        {modkey, "Control"},
        "j",
        function()
            awful.screen.focus_relative(1)
        end,
        {description = "focus the next screen", group = "screen"}
    ),
    awful.key(
        {modkey, "Control"},
        "k",
        function()
            awful.screen.focus_relative(-1)
        end,
        {description = "focus the previous screen", group = "screen"}
    ),
    awful.key(
        {modkey, "Shift"},
        "u",
        awful.client.urgent.jumpto,
        {description = "jump to urgent client", group = "client"}
    ),
    -- Commented out the old modkey+Tab shortcut in favor of the awesome
    -- cyclefocus plugin.
    -- awful.key({ modkey,           }, "Tab",
    --     function ()
    --         awful.client.focus.history.previous()
    --         if client.focus then
    --             client.focus:raise()
    --         end
    --     end,
    --     {description = "go back", group = "client"}),

    -- modkey+Tab: cycle through all clients.
    awful.key(
        {modkey},
        "Tab",
        function(c)
            cyclefocus.cycle({modifier = "Super_L"})
        end
    ),
    -- modkey+Shift+Tab: backwards
    awful.key(
        {modkey, "Shift"},
        "Tab",
        function(c)
            cyclefocus.cycle({modifier = "Super_L"})
        end
    ),
    -- Standard program
    awful.key(
        {modkey},
        "Return",
        function()
            awful.spawn(terminal)
        end,
        {description = "open a terminal", group = "launcher"}
    ),
    awful.key(
        {modkey},
        "e",
        function()
            awful.spawn("emacs")
        end,
        {description = "open emacs", group = "launcher"}
    ),
    awful.key(
        {modkey, "Shift"},
        "f",
        function()
            awful.spawn(browser)
        end,
        {description = "open " .. browser, group = "launcher"}
    ),
    awful.key({modkey, "Control"}, "r", awesome.restart, {description = "reload awesome", group = "awesome"}),
    awful.key({modkey, "Shift"}, "q", awesome.quit, {description = "quit awesome", group = "awesome"}),
    awful.key(
        {modkey},
        "l",
        function()
            awful.tag.incmwfact(0.05)
        end,
        {description = "increase master width factor", group = "layout"}
    ),
    awful.key(
        {modkey},
        "h",
        function()
            awful.tag.incmwfact(-0.05)
        end,
        {description = "decrease master width factor", group = "layout"}
    ),
    awful.key(
        {modkey, "Shift"},
        "h",
        function()
            awful.tag.incnmaster(1, nil, true)
        end,
        {description = "increase the number of master clients", group = "layout"}
    ),
    awful.key(
        {modkey, "Shift"},
        "l",
        function()
            awful.tag.incnmaster(-1, nil, true)
        end,
        {description = "decrease the number of master clients", group = "layout"}
    ),
    awful.key(
        {modkey, "Control"},
        "h",
        function()
            awful.tag.incncol(1, nil, true)
        end,
        {description = "increase the number of columns", group = "layout"}
    ),
    awful.key(
        {modkey, "Control"},
        "l",
        function()
            awful.tag.incncol(-1, nil, true)
        end,
        {description = "decrease the number of columns", group = "layout"}
    ),
    awful.key(
        {modkey},
        "space",
        function()
            awful.layout.inc(1)
        end,
        {description = "select next", group = "layout"}
    ),
    awful.key(
        {modkey, "Shift"},
        "space",
        function()
            awful.layout.inc(-1)
        end,
        {description = "select previous", group = "layout"}
    ),
    awful.key(
        {modkey, "Control"},
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
        {modkey},
        "r",
        function()
            awful.spawn("rofi -show run")
        end,
        {description = "run a generic command", group = "launcher"}
    ),
    awful.key(
        {modkey},
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
        {modkey},
        "p",
        function()
            awful.spawn("rofi -show drun")
        end,
        {description = "launch an application", group = "launcher"}
    ),
    awful.key(
        {modkey, "Shift"},
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
        {modkey, "Shift", "Control"},
        "l",
        function()
            awful.spawn("xscreensaver-command -lock")
        end,
        {description = "Lock the display", group = "system"}
    ),
    awful.key(
        {modkey, "Shift", "Control"},
        "s",
        function()
            awful.spawn("systemctl suspend")
        end,
        {description = "Put computer to sleep", group = "system"}
    )
)

local clientkeys =
    gears.table.join(
    awful.key(
        {modkey},
        "f",
        function(c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end,
        {description = "toggle fullscreen", group = "client"}
    ),
    awful.key(
        {modkey, "Shift"},
        "c",
        function(c)
            c:kill()
        end,
        {description = "close", group = "client"}
    ),
    awful.key(
        {modkey, "Control"},
        "space",
        awful.client.floating.toggle,
        {description = "toggle floating", group = "client"}
    ),
    awful.key(
        {modkey, "Control"},
        "Return",
        function(c)
            c:swap(awful.client.getmaster())
        end,
        {description = "move to master", group = "client"}
    ),
    awful.key(
        {modkey},
        "o",
        function(c)
            c:move_to_screen()
        end,
        {description = "move to screen", group = "client"}
    ),
    awful.key(
        {modkey},
        "t",
        function(c)
            c.ontop = not c.ontop
        end,
        {description = "toggle keep on top", group = "client"}
    ),
    awful.key(
        {modkey},
        "n",
        function(c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end,
        {description = "minimize", group = "client"}
    ),
    awful.key(
        {modkey},
        "m",
        function(c)
            c.maximized = not c.maximized
            c:raise()
        end,
        {description = "(un)maximize", group = "client"}
    ),
    awful.key(
        {modkey, "Control"},
        "m",
        function(c)
            c.maximized_vertical = not c.maximized_vertical
            c:raise()
        end,
        {description = "(un)maximize vertically", group = "client"}
    ),
    awful.key(
        {modkey, "Shift"},
        "m",
        function(c)
            c.maximized_horizontal = not c.maximized_horizontal
            c:raise()
        end,
        {description = "(un)maximize horizontally", group = "client"}
    )
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
    globalkeys =
        gears.table.join(
        globalkeys,
        -- View tag only.
        awful.key(
            {modkey},
            "#" .. i + 9,
            function()
                local screen = awful.screen.focused()
                local tag = screen.tags[i]
                if tag then
                    tag:view_only()
                end
            end,
            {description = "view tag #" .. i, group = "tag"}
        ),
        -- Toggle tag display.
        awful.key(
            {modkey, "Control"},
            "#" .. i + 9,
            function()
                local screen = awful.screen.focused()
                local tag = screen.tags[i]
                if tag then
                    awful.tag.viewtoggle(tag)
                end
            end,
            {description = "toggle tag #" .. i, group = "tag"}
        ),
        -- Move client to tag.
        awful.key(
            {modkey, "Shift"},
            "#" .. i + 9,
            function()
                if client.focus then
                    local tag = client.focus.screen.tags[i]
                    if tag then
                        client.focus:move_to_tag(tag)
                    end
                end
            end,
            {description = "move focused client to tag #" .. i, group = "tag"}
        ),
        -- Toggle tag on focused client.
        awful.key(
            {modkey, "Control", "Shift"},
            "#" .. i + 9,
            function()
                if client.focus then
                    local tag = client.focus.screen.tags[i]
                    if tag then
                        client.focus:toggle_tag(tag)
                    end
                end
            end,
            {description = "toggle focused client on tag #" .. i, group = "tag"}
        )
    )
end

local clientbuttons =
    gears.table.join(
    awful.button(
        {},
        1,
        function(c)
            c:emit_signal("request::activate", "mouse_click", {raise = true})
        end
    ),
    awful.button(
        {modkey},
        1,
        function(c)
            c:emit_signal("request::activate", "mouse_click", {raise = true})
            awful.mouse.client.move(c)
        end
    ),
    awful.button(
        {modkey},
        3,
        function(c)
            c:emit_signal("request::activate", "mouse_click", {raise = true})
            awful.mouse.client.resize(c)
        end
    )
)

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
    -- All clients will match this rule.
    {
        rule = {},
        properties = {
            border_width = beautiful.border_width,
            border_color = beautiful.border_normal,
            focus = awful.client.focus.filter,
            raise = true,
            keys = clientkeys,
            buttons = clientbuttons,
            screen = awful.screen.preferred,
            placement = awful.placement.no_overlap + awful.placement.no_offscreen
        }
    },
    -- Floating clients.
    {
        rule_any = {
            instance = {
                "DTA", -- Firefox addon DownThemAll.
                "copyq", -- Includes session name in class.
                "pinentry"
            },
            class = {
                "Arandr",
                "Blueman-manager",
                "Gpick",
                "Kruler",
                "MessageWin", -- kalarm.
                "Sxiv",
                "Tor Browser", -- Needs a fixed window size to avoid fingerprinting by screen size.
                "Wpa_gui",
                "veromix",
                "xtightvncviewer"
            },
            -- Note that the name property shown in xprop might be set slightly after creation of the client
            -- and the name shown there might not match defined rules here.
            name = {
                "Event Tester" -- xev.
            },
            role = {
                "AlarmWindow", -- Thunderbird's calendar.
                "ConfigManager", -- Thunderbird's about:config.
                "pop-up" -- e.g. Google Chrome's (detached) Developer Tools.
            }
        },
        properties = {floating = true}
    },
    -- Add titlebars to normal clients and dialogs
    {
        rule_any = {
            type = {"normal", "dialog"}
        },
        properties = {titlebars_enabled = nil}
    }

    -- Set Firefox to always map on the tag named "2" on screen 1.
    -- { rule = { class = "Firefox" },
    --   properties = { screen = 1, tag = "2" } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal(
    "manage",
    function(c)
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
        -- if not awesome.startup then awful.client.setslave(c) end

        if awesome.startup and not c.size_hints.user_position and not c.size_hints.program_position then
            -- Prevent clients from being unreachable after screen count changes.
            awful.placement.no_offscreen(c)
        end
    end
)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal(
    "request::titlebars",
    function(c)
        -- buttons for the titlebar
        local buttons =
            gears.table.join(
            awful.button(
                {},
                1,
                function()
                    c:emit_signal("request::activate", "titlebar", {raise = true})
                    awful.mouse.client.move(c)
                end
            ),
            awful.button(
                {},
                3,
                function()
                    c:emit_signal("request::activate", "titlebar", {raise = true})
                    awful.mouse.client.resize(c)
                end
            )
        )

        awful.titlebar(c):setup {
            {
                -- Left
                awful.titlebar.widget.iconwidget(c),
                buttons = buttons,
                layout = wibox.layout.fixed.horizontal
            },
            {
                -- Middle
                {
                    -- Title
                    align = "center",
                    widget = awful.titlebar.widget.titlewidget(c)
                },
                buttons = buttons,
                layout = wibox.layout.flex.horizontal
            },
            {
                -- Right
                awful.titlebar.widget.floatingbutton(c),
                awful.titlebar.widget.maximizedbutton(c),
                awful.titlebar.widget.stickybutton(c),
                awful.titlebar.widget.ontopbutton(c),
                awful.titlebar.widget.closebutton(c),
                layout = wibox.layout.fixed.horizontal()
            },
            layout = wibox.layout.align.horizontal
        }
    end
)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal(
    "mouse::enter",
    function(c)
        c:emit_signal("request::activate", "mouse_enter", {raise = false})
    end
)

client.connect_signal(
    "focus",
    function(c)
        c.border_color = beautiful.border_focus
    end
)
client.connect_signal(
    "unfocus",
    function(c)
        c.border_color = beautiful.border_normal
    end
)
-- }}}

-- Polybar Integration {{{
-- Launch polybar
-- os.execute("$HOME/.config/polybar/launch.sh")
-- }}}

--- Autorun programs {{{
-- Whether or not to start apps on startup
local autorun = true

-- List of apps to run on startup
local autorunApps = {
    -- Terminals
    {
        name = terminal,
        opts = "",
        tag = "main"
    },
    {
        name = terminal,
        opts = "",
        tag = "main"
    },
    {
        name = terminal,
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
        name = browser,
        opts = "",
        tag = "www"
    }
}

if autorun then
    -- Iterator for-loop construct in Lua
    for i, app in ipairs(autorunApps) do
        local appString = app["name"]
        local opts = app["opts"]

        if opts ~= nil and opts ~= "" then
            appString = appString .. " " .. opts
        end

        awful.spawn(
            appString,
            {
                tag = app["tag"]
            }
        )
    end
end
-- }}}

-- vim:foldenable:foldmethod=marke
