local os_util = {}

-- Pull in the standard OS library functions from the external posix library.
local stdlib = require("posix.stdlib")

--- Executes a shell command and returns the output as a string.
-- @param cmd The command string to execute
function os_util.cmd_to_string(cmd)
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

--- Adds one or more new directories to the $PATH environment variable
function os_util.add_to_path(...)
    -- Add extra executable paths to $PATH
    local path
    local new_path = ''
    for i = 1, select('#', ...) do
        path = select(i, ...)
        if i == 1 then
            new_path = path
        else
            new_path = path .. ':' .. new_path
        end
    end
    stdlib.setenv("PATH", new_path .. ":" .. os.getenv("PATH"))
end

return os_util
