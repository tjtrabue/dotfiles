-- Configuration for nvim-lint, a code linter that acts in concert with Neovim's
-- built-in LSP client.

-- Set linters by filetype
require("lint").inters_by_ft = {
  ansible = {"ansible_lint"},
  bash = {"shellcheck"},
  c = {"cppcheck"},
  clojure = {"clj-kondo"},
  cmake = {"cmakelint"},
  cpp = {"cppcheck"},
  dockerfile = {"hadolint"},
  go = {"revive"},
  haskell = {"hlint"},
  html = {"tidy"},
  java = {"checkstyle"},
  javascript = {"eslint"},
  javascriptreact = {"eslint"},
  lua = {"selene"},
  markdown = {"markdownlint"},
  python = {"pylint"},
  ruby = {"ruby"},
  sh = {"shellcheck"},
  tex = {"chktex"},
  vim = {"vint"},
  yaml = {"yamllint"},
  zsh = {"shellcheck"}
}

-- Set up autocmd to trigger linting
vim.api.nvim_exec([[
  autocmd BufWritePost <buffer> lua require('lint').try_lint()
  ]], true)

-- vim:foldenable:foldmethod=marker:foldlevel=0
