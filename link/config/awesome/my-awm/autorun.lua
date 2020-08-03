local awful = require("awful")

--- Start applications automatically when Awesome initializes.
-- @param autorun_apps Table of GUI applications to run on startup.
-- Each element of the table is itself a table with the following structure:
--   name - String; the client application's name.
--   opts - String; any additional cli args for the command.
--   tag  - String; the name of the tag in which to open the client.
local function autorun(autorun_apps)
    -- Iterator for-loop construct in Lua
    for i, app in ipairs(autorun_apps) do
        local app_string = app["name"]
        local opts = app["opts"]

        if opts ~= nil and opts ~= "" then
            app_string = app_string .. " " .. opts
        end

        awful.spawn(
            app_string,
            {
                tag = app["tag"]
            }
        )
    end
end

return autorun
