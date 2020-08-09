-- Create a list of all available themes for selection by the user

-- Main imports
local lfs = require("lfs")
local gfs = require("gears.filesystem")

-- Local imports
local paths = require("my-awm.paths")
local theme_vars = require("my-awm.theme.theme-vars")

local menu_items = {}

-- List of search paths
local path_list = {
    paths.themes_custom_dir,
    paths.themes_system_dir
}

-- Initialize table
local theme_list = {}

-- Perform search
for _, path in ipairs(path_list) do
    for fold in lfs.dir(path) do
        local f_attr = lfs.attributes(path .. "/" .. fold, "mode")
        if f_attr and f_attr == "directory" and fold ~= "." and fold ~= ".." then
            fname_full = path .. "/" .. fold .. "/" .. theme_vars.theme_file
            if gfs.file_readable(fname_full) then
                if not theme_list[fold] then
                    theme_list[fold] = path .. "/" .. fold .. "/" .. theme_vars.theme_file
                end
            end
        end
    end
end

-- Create menu
for theme_name, theme_file in pairs(theme_list) do
    theme = nil
    theme = dofile(theme_file)
    theme_icon = theme.awesome_icon
    theme = nil
    table.insert(
        menu_items,
        {
            theme_name,
            function()
                local theme_fname = paths.themes_custom_dir .. "/theme"
                local file = io.open(theme_fname, "w")
                file:write(theme_name .. "\n")
                file:close()
                awesome.restart()
            end,
            theme_icon
        }
    )
end

return menu_items
