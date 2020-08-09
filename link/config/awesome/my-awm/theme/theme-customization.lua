--- Theme helper functions.
--- Adapted from thibaultmarin's dotfiles.

-- Main imports
local dofile = dofile
local lfs = require("lfs")
local awful = require("awful")
local beautiful = require("beautiful")
local gfs = require("gears.filesystem")

-- Local imports
local paths = require("my-awm.paths")
local theme_vars = require("my-awm.theme.theme-vars")

local theme_customization = {}

-- @return the first file found on a table of file paths.
local function get_first_found_file(path_list, fname)
    local fname_full
    for _, f in ipairs(path_list) do
        fname_full = f .. "/" .. fname
        if gfs.file_readable(fname_full) then
            return fname_full
        end
    end
    return nil
end

-- @return The name of the Awesome WM theme to use from the custom theme file
function theme_customization.get_theme_name_from_file()
    local theme_fname = paths.themes_custom_file
    local theme_name = "default"
    if gfs.file_readable(theme_fname) then
        local f = io.open(theme_fname, "r")
        for line in f:lines() do
            if string.find(line, "^%s*[^#]") ~= nil then
                theme_name = line:gsub("^%s*(.-)%s*$", "%1")
            end
        end
        f:close()
    end
    return theme_name
end

-- Set the Awesome WM theme.
function theme_customization.set_custom_theme(theme)
    -- Locate theme file
    local path_list = {
        paths.themes_custom_dir .. "/" .. theme,
        paths.themes_system_dir .. "/" .. theme
    }
    local fname_theme = get_first_found_file(path_list, theme_vars.theme_file)
    if not fname_theme then
        return
    end

    -- Locate customization file
    local fname_custom = get_first_found_file(path_list, theme_vars.custom_file)

    -- Initialize beautiful theme
    beautiful.init(fname_theme)

    -- Load default customization (fields required by rc.lua not defined by
    -- regular themes)
    local fname_custom_defaults = paths.themes_custom_dir .. "/custom_defaults.lua"
    local custom_defaults = {}
    if gfs.file_readable(fname_custom_defaults) then
        -- `custom_defaults` table
        custom_defaults = dofile(fname_custom_defaults)
    end

    -- Load theme customization
    local custom = {a = "b"}
    if fname_custom then
        custom = dofile(fname_custom) -- `custom` table
    end

    -- Join all tables
    local custom_tables = {custom_defaults, custom}
    for _, t in ipairs(custom_tables) do
        for key, value in pairs(t) do
            beautiful[key] = value
        end
    end
end

return theme_customization
