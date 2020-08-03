local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local taglist_buttons = require("my-awm.taglist.taglist-buttons")

local taglist_util = {}

function taglist_util.connect_taglist_to_screen(s)
    local taglist =
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
    return taglist
end

return taglist_util
