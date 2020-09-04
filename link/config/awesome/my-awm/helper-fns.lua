local paths = require("my-awm.paths")

-- Helper functions for use in configuring Awesome WM.
local helpers = {}

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

-- Adds a given path string to the end of the package.path global variable.
-- @param path_var The name of the package path variable to use (i.e., package.path)
-- @param path The path to add to the path variable
local function add_to_path(path_var, path)
    path_var = path_var or "package.path"
    -- Create a string containing the Lua code for the package path assignment
    -- statement.
    local path_assignment_code = path_var .. " = " .. path_var .. " .. " .. "';" .. path .. "'"
    local assignment_func = load(path_assignment_code)
    -- Execute the assignment.
    assignment_func()
end

--[[
  Adds paths for user installed LuaRocks packages to package.path.
  This function exists to cover up a gaping flaw in the LuaRocks system,
  which is that the luarocks.loader module only works for the latest version
  of lua, not for any older installations you may have on your computer.
  Thus, if we have lua 5.4 installed but we want to use user installed
  packages for lua 5.3 then we have to use this manual hack.

  @param lua_version The lua version to add LuaRocks for (such as '5.3')
--]]
function helpers.add_luarocks_paths(lua_version)
    local luarocks_share_home = paths.luarocks_user_dir .. "/share/lua/" .. lua_version
    local luarocks_lib_home = paths.luarocks_user_dir .. "/lib/lua/" .. lua_version

    -- Add important lib paths to package.path
    add_to_path("package.path", luarocks_share_home .. "/?.lua")
    add_to_path("package.path", luarocks_share_home .. "/?/init.lua")
    -- Add extra dynamically linked libraries to package.cpath
    add_to_path("package.cpath", luarocks_lib_home .. "/?.so")
end

-- Checks for the existence of a directory based on its path.
-- @param path The directory path
function helpers.directory_exists(path)
    return file_or_dir_exists(path .. "/")
end

-- Checks for the existence of an input file path.
-- @param path The file path to check
function helpers.file_exists(path)
    return file_or_dir_exists(path) and not helpers.directory_exists(path)
end

--- Executes a shell command and returns the output as a string.
-- @param cmd The command string to execute
function helpers.cmd_to_string(cmd)
    -- The output string to return
    local str = ""
    -- get a temporary file name
    local tmp = os.tmpname()

    -- execute a command
    os.execute(cmd .. " > " .. tmp)

    -- display output
    for line in io.lines(tmp) do
        if str ~= "" then
            str = str .. "\n"
        end
        str = str .. line
    end

    -- remove temporary file
    os.remove(tmp)

    return str
end

return helpers
