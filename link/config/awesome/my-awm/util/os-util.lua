local os_util = {}

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

return os_util
