-- Helper functions for use in configuring Awesome WM.
local helpers = {}

-- Adds a given path string to the end of the package.path global variable.
-- @param path The path to add to package.path
local function add_to_package_path(path)
    package.path = package.path .. ";" .. path
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
    local luarocks_user_home = os.getenv("HOME") .. "/.luarocks"
    local luarocks_lib_home = luarocks_user_home .. "/share/lua/" .. lua_version
    -- Add important lib paths to package.path
    add_to_package_path(luarocks_lib_home .. "/?.lua")
    add_to_package_path(luarocks_lib_home .. "/?/init.lua")
end

-- Checks for the existence of an input file path.
-- @param path The file path to check.
function helpers.file_exists(path)
    local f = io.open(path, "r")
    if f ~= nil then
        io.close(f)
        return true
    else
        return false
    end
end

return helpers
