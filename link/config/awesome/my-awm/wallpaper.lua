-- Wallpaper configuration

local gears = require("gears")

local my_vars = require("my-awm.vars")

-- Set according to wallpaper directory
local wallpaper_dir_path = os.getenv("HOME") .. "/wallpaper/1920x1080/"
-- Other variables
local wp_all = {}
local wp_selected = {}

local wp_util = {}

math.randomseed(os.time())
-- To guarantee unique random numbers on every platform, pop a few
for i = 1, 10 do
    math.random()
end

--- Return the number of files in a directory.
-- @param directory The directory to use for counting files.
local function get_num_files(directory)
    local nfiles = 0
    for filename in io.popen('ls -a "' .. directory .. '"'):lines() do
        -- If case to disregard "." and ".."
        if (not (filename == "." or filename == "..")) then
            nfiles = nfiles + 1
        end
    end
    return nfiles
end

--- LUA implementation of PHP scan dir
--- Returns all files (except . and ..) in "directory"
-- @param directory The directory to scan for files
local function scandir(directory)
    local t = {}
    local i = 0
    for filename in io.popen('ls -a "' .. directory .. '"'):lines() do
        -- If case to disregard "." and ".."
        if (not (filename == "." or filename == "..")) then
            t[i] = filename
            i = i + 1
        end
    end
    return t
end

--- Basically a modern Fisher-Yates shuffle
--- Returns "tags" elements from a table "wp" of length "nfiles"
--- Guarantees no duplicated elements in the return while having linear runtime
-- @param wp The wallpaper image file array
-- @param nfiles The files array
-- @param tags The array of tags
local function select_wallpaper(wp, nfiles, tags)
    local selected = {}
    local position
    for i = 1, tags do
        position = math.random(1, nfiles)
        selected[i] = wp[position]
        wp[position] = wp[nfiles]
        nfiles = nfiles - 1
    end
    return selected
end

-- Get the names of "my_vars.num_tags" files from "num_files" total files in the
-- wallpaper directory path.
wp_selected = select_wallpaper(scandir(wallpaper_dir_path), get_num_files(wallpaper_dir_path), my_vars.num_tags)

--- Randomly sets wallpaper for each tag in a screen from a wallpaper directory.
-- @param scr Screen variable
function wp_util.set_wallpaper(scr)
    -- Set wallpaper on first tag (else it would be empty at start up)
    gears.wallpaper.fit(wallpaper_dir_path .. wp_selected[1], scr)
    -- Go over each tag
    for t = 1, my_vars.num_tags do
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
screen.connect_signal("property::geometry", wp_util.set_wallpaper)

return wp_util
