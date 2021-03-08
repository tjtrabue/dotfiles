-- Default programs to use for various applications.

local default_progs = {}

-- Figure out web browser.
if os.execute("command -v brave") then
    -- Use Brave, which is like Chromium but better for privacy.
    default_progs.browser = "brave"
elseif os.execute("command -v chromium") then
    -- Next best is Chromium.
    default_progs.browser = "chromium"
else
    -- Default to Firefox (the traitorous thing...)
    default_progs.browser = "firefox"
end

-- Figure out terminal program.
if os.execute("command -v kitty") then
    default_progs.terminal = "kitty"
else
    default_progs.terminal = "alacritty"
end

default_progs.music = "cmus"
default_progs.editor = os.getenv("EDITOR") or "vim"
default_progs.editor_cmd = default_progs.terminal .. " -e " .. default_progs.editor
default_progs.explorer = "xdg-open"

return default_progs
