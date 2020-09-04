-- Variables to use throughout Awesome

local vars = {}

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
vars.modkey = "Mod4"

-- Names for the workspaces
vars.tag_names = {
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

vars.num_tags = #vars.tag_names

return vars
