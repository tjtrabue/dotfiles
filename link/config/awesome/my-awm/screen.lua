-- Require Awesome WM libraries
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

-- Load additional widgets from awesome-wm-widgets repo (must be cloned)
local battery_widget = require("awesome-wm-widgets.batteryarc-widget.batteryarc")
local volume_widget = require("awesome-wm-widgets.volume-widget.volume")
local ram_widget = require("awesome-wm-widgets.ram-widget.ram-widget")
local cpu_widget = require("awesome-wm-widgets.cpu-widget.cpu-widget")
local brightness_widget = require("awesome-wm-widgets.brightness-widget.brightness")
local weather_widget = require("awesome-wm-widgets.weather-widget.weather")

-- Create a calendar widget anchored to the default textclock widget
local calendar_widget = require("awesome-wm-widgets.calendar-widget.calendar")
local mytextclock = wibox.widget.textclock()
local cw = calendar_widget()
mytextclock:connect_signal(
    "button::press",
    function(_, _, _, button)
        if button == 1 then
            cw.toggle()
        end
    end
)

-- Local imports
local my_vars = require("my-awm.vars")
local wp_util = require("my-awm.wallpaper")
local taglist_util = require("my-awm.taglist.taglist")
-- Buttons to use for viewing each client
local tasklist_buttons = require("my-awm.tasklist.tasklist-buttons")
-- The Awesome button in the top left hand corner of the screen.
local awesome_launcher_button = require("my-awm.widgets.launcher")

local screen_util = {}

-- Connects a screen to the Awesome WM environment, adding a wibar, widgets,
-- layouts, etc.
function screen_util.connect_screen(s)
    local l = awful.layout.suit
    local layouts = {}
    -- Fill in the layouts table with appropriate layouts.
    for i = 1, my_vars.num_tags do
        layouts[i] = l.tile
    end
    awful.tag(my_vars.tag_names, s, layouts)

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
    s.mytaglist = taglist_util.connect_taglist_to_screen(s)

    -- Wallpaper
    wp_util.set_wallpaper(s)

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
            awesome_launcher_button,
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

return screen_util
