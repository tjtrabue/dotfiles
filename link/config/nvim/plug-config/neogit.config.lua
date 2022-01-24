-- Neogit is a Magit clone for Neovim.

local neogit = require("neogit")

neogit.setup {
  disable_commit_confirmation = true,
  signs = {
    -- { CLOSED, OPENED }
    section = {"ᐅ", "ᐁ"},
    item = {"ᐅ", "ᐁ"},
    hunk = {"", ""}
  }
}
