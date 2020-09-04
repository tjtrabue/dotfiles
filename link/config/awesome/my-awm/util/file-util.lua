local file_util = {}

--- Local definitions ---

--- Check if a file or directory exists in this path
-- @param path The file/directory path to check
local function file_or_dir_exists(path)
    local ok, _, code = os.rename(path, path)
    if not ok then
        if code == 13 then
            -- Permission denied, but it exists
            return true
        end
    end
    return ok
end

--- Public API ---

-- Checks for the existence of a directory based on its path.
-- @param path The directory path
function file_util.directory_exists(path)
    return file_or_dir_exists(path .. "/")
end

-- Checks for the existence of an input file path.
-- @param path The file path to check
function file_util.file_exists(path)
    return file_or_dir_exists(path) and not file_util.directory_exists(path)
end

--- Return the number of files in a directory.
-- @param directory The directory to use for counting files.
function file_util.get_num_files(directory)
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
function file_util.scandir(directory)
    local files = {}
    local i = 0
    for filename in io.popen('ls -a "' .. directory .. '"'):lines() do
        -- If case to disregard "." and ".."
        if (not (filename == "." or filename == "..")) then
            files[i] = filename
            i = i + 1
        end
    end
    return files
end

return file_util
