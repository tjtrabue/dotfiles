local awful = require("awful")

--- Start applications automatically when Awesome initializes.
--- Apps started with autorun will only run once. They will not be run if
--- awesome restarts.
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
    -- Sometimes a unique ID is necessary to spawn multiple instances of the
    -- same app.
    local id = app["id"]
    local rules = {}

    rules["tag"] = app["tag"]

    if opts ~= nil and opts ~= "" then
      app_string = app_string .. " " .. opts
    end

    if id ~= nil and id ~= "" then
      awful.spawn.once(
        app_string,
        rules,
        function(c)
          return c.startup_id == id
        end,
        id
      )
    else
      awful.spawn.once(app_string, rules)
    end
  end
end

return autorun
