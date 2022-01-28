-- Filesystem functions.

local fs = {}

--- Execute the OS command `cmd` and return the result as a string.
function fs.os_cmd_to_string(cmd)
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

return fs
