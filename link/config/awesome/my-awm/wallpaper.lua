-- Wallpaper configuration

local gears = require("gears")

local my_vars = require("my-awm.vars")
local helpers = require("my-awm.helper-fns")

--- Set screen resolution
-- This command returns the screen's resolution dynamically
local resolution =
    helpers.cmd_to_string(
    "xrandr | grep 'Screen 0' | awk -F ',' '{ print $2 }' | egrep -o '[0-9]+ *x *[0-9]+' | tr -d ' ,'"
)
-- This is the hard-coded resolution (mostly for testing)
-- local resolution = "1920x1080"

-- Set according to wallpaper directory
local wallpaper_dir_path = os.getenv("HOME") .. "/wallpaper/" .. resolution .. "/"
-- Total number of wallpaper image files
local num_wallpaper_files
-- List of selected wallpaper image files
local wp_selected = nil

local wp_util = {}

print("Screen Resolution: " .. resolution)

math.randomseed(os.time())
-- To guarantee unique random numbers on every platform, pop a few
for _ = 1, 10 do
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

-- If we have a valid wallpaper directory that contains at least as many image
-- files as we have tags, create a random list of wallpaper images for use as
-- desktop backgrounds.
if helpers.directory_exists(wallpaper_dir_path) then
    num_wallpaper_files = get_num_files(wallpaper_dir_path)
    if num_wallpaper_files >= my_vars.num_tags then
        wp_selected = select_wallpaper(scandir(wallpaper_dir_path), num_wallpaper_files, my_vars.num_tags)
    end
end

--- Set the desktop wallpaper for a specific tag, given its screen and index.
-- @param scr The screen to which the tag is attached
-- @param index The tag's index within the screen
local function set_wallpaper_for_tag(scr, index)
    local default_wp = gears.filesystem.get_themes_dir() .. "/default/background.png"

    if wp_selected then
        -- Use a randomly selected image if we have a valid
        -- wallpaper directory
        gears.wallpaper.fit(wallpaper_dir_path .. wp_selected[index], scr)
    else
        -- Otherwise, just use the default wallpaper image.
        gears.wallpaper.fit(default_wp, scr)
    end
end

--- Randomly sets wallpaper for each tag in a screen from a wallpaper directory.
-- @param scr Screen variable
function wp_util.set_wallpaper(scr)
    -- Set wallpaper on first tag (else it would be empty at start up)
    set_wallpaper_for_tag(scr, 1)

    -- Go over each tag
    for index = 1, my_vars.num_tags do
        scr.tags[index]:connect_signal(
            "property::selected",
            function(tag)
                -- And if selected
                if not tag.selected then
                    return
                end
                -- Set wallpaper
                set_wallpaper_for_tag(scr, index)
            end
        )
    end
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", wp_util.set_wallpaper)

return wp_util
