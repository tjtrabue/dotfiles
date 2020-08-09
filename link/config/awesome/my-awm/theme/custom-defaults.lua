--- Custom theme default values.

local beautiful = require("beautiful")

local custom_defaults = {}

-- mail widget
custom_defaults.mail_fg_urgent = beautiful.fg_urgent
custom_defaults.mail_fg_normal = beautiful.fg_normal
custom_defaults.mail_fg_focus = beautiful.fg_focus

-- memory widget
custom_defaults.mem_bg = beautiful.bg_normal

-- cmus widget
custom_defaults.cmus_fg = beautiful.fg_normal

-- screen highlighter
custom_defaults.screen_highlight_bg_active = beautiful.bg_minimize
custom_defaults.screen_highlight_fg_active = beautiful.fg_minimize
custom_defaults.screen_highlight_bg_inactive = beautiful.bg_normal
custom_defaults.screen_highlight_fg_inactive = beautiful.fg_normal

return custom_defaults
