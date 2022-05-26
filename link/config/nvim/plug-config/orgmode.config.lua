-- Configuration for the Org mode clone for Neovim.

-- Load custom tree-sitter grammar for org filetype
require("orgmode").setup_ts_grammar()

require("orgmode").setup {
  org_agenda_files = {"~/.emacs.d/org/agenda/*"},
  org_default_notes_file = "~/.emacs.d/org/refile.org"
}

-- Keymappings {{{
-- Org agenda
vim.api.nvim_set_keymap("n", "<leader>oa", "<cmd>org_agenda<cr>", {silent = true, noremap = true})
-- Org capture
vim.api.nvim_set_keymap("n", "<leader>oc", "<cmd>org_capture<cr>", {silent = true, noremap = true})
-- }}}

-- vim:foldenable:foldmethod=marker:foldlevel=0
