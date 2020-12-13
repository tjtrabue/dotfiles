-- Wallpaper configuration

local gears = require("gears")

local my_vars = require("my-awm.vars")
local file_util = require("my-awm.util.file-util")
-- local os_util = require("my-awm.util.os-util")

--- Set screen resolution
--- Currently unused because it's a little overkill. I've come to believe that
--- it's better to have a single wallpaper directory with whatever quality
--- images you prefer and just deal with it. Trying to dynamically figure out
--- which images you need based on your screen size is a very difficult problem
--- to get right, especially if you have multiple monitors of different sizes.
-- This command returns the screen's resolution dynamically.
-- Should be formatted like '1920x1080'.
-- local resolution = os_util.cmd_to_string("xrandr | fgrep '*' | head -1 | awk '{ print $1 }'")
-- This is the hard-coded resolution (mostly for testing)
-- local resolution = "1920x1080"

-- Set according to wallpaper directory
local wallpaper_dir_path = os.getenv("HOME") .. "/wallpaper/"
-- Total number of wallpaper image files
local num_wallpaper_files
-- List of selected wallpaper image files
local wp_selected = nil

local wp_util = {}

math.randomseed(os.time())
-- To guarantee unique random numbers on every platform, pop a few
for _ = 1, 10 do
    math.random()
end

--- Basically a modern Fisher-Yates shuffle
--- Returns "tags" elements from a table "wp" of length "nfiles"
--- Guarantees no duplicated elements in the return while having linear runtime
-- @param wp The wallpaper image file array
-- @param num_files The number of image files in the wallpaper directory
-- @param num_tags Number of tags per screen
local function select_wallpaper(wp, num_files, num_tags)
    local selected = {}
    local position
    for i = 1, num_tags do
        position = math.random(1, num_files)
        selected[i] = wp[position]
        wp[position] = wp[num_files]
        num_files = num_files - 1
    end
    return selected
end

-- If we have a valid wallpaper directory that contains at least as many image
-- files as we have tags, create a random list of wallpaper images for use as
-- desktop backgrounds.
if file_util.directory_exists(wallpaper_dir_path) then
    print("Found wallpaper directory")
    num_wallpaper_files = file_util.get_num_files(wallpaper_dir_path)
    if num_wallpaper_files >= my_vars.num_tags then
        wp_selected = select_wallpaper(file_util.scandir(wallpaper_dir_path), num_wallpaper_files, my_vars.num_tags)
        print("Successfully retrieved wallpaper images")
    else
        print("Not enough wallpaper images in directory")
    end
else
    print("No wallpaper directory found")
end

--- Set the desktop wallpaper for a specific tag, given its screen and index.
-- @param scr The screen to which the tag is attached
-- @param index The tag's index within the screen (starting at 1)
local function set_wallpaper_for_tag(scr, index)
    local default_wp = gears.filesystem.get_themes_dir() .. "/default/background.png"

    if wp_selected then
        print("Using custom wallpaper")
        -- Use a randomly selected image if we have a valid
        -- wallpaper directory
        gears.wallpaper.fit(wallpaper_dir_path .. wp_selected[index], scr)
    else
        print("Using default wallpaper")
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
