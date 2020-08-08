-- Default programs to use for various applications.

local default_progs = {}

default_progs.terminal = "alacritty"
default_progs.browser = "firefox"
default_progs.editor = os.getenv("EDITOR") or "vim"
default_progs.editor_cmd = default_progs.terminal .. " -e " .. default_progs.editor
default_progs.explorer = "xdg-open"

return default_progs
