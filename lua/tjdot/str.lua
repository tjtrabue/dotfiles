-- String functions.

local str = {}

-- Test if a string is empty or nil.
function str.isempty(s)
  return s == nil or s == ""
end

return str
