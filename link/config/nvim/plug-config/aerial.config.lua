-- Aerial is a code browser for Neovim that provides great integration with
-- Neovim's built-in LSP client.
require("aerial").setup(
  {
    layout = {
      -- Enum: prefer_right, prefer_left, right, left
      -- Determines the default direction to open the aerial window. The 'prefer'
      -- options will open the window in the other direction *if* there is a
      -- different buffer in the way of the preferred direction
      default_direction = "prefer_left"
    }
  }
)

vim.keymap.set("n", "<leader>a", "<cmd>AerialToggle!<CR>")

-- vim:foldenable:foldmethod=marker:foldlevel=0
