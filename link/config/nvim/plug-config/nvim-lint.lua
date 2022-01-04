-- Configuration for nvim-lint, a code linter that acts in concert with Neovim's
-- built-in LSP client.

-- Set linters by filetype
require("lint").inters_by_ft = {
  ansible = {"ansible_lint", "cspell"},
  bash = {"shellcheck", "cspell"},
  c = {"cppcheck", "cspell"},
  clojure = {"clj-kondo", "cspell"},
  cmake = {"cmakelint"},
  cpp = {"cppcheck", "cpplint", "cspell"},
  dockerfile = {"hadolint"},
  go = {"revive", "cspell"},
  haskell = {"hlint", "cspell"},
  html = {"tidy", "cspell"},
  java = {"checkstyle", "cspell"},
  javascript = {"eslint", "cspell"},
  javascriptreact = {"eslint", "cspell"},
  lua = {"selene", "luacheck", "cspell"},
  markdown = {"markdownlint", "vale", "proselint"},
  python = {"pylint", "flake8", "cspell"},
  ruby = {"ruby", "cspell"},
  sh = {"shellcheck", "cspell"},
  tex = {"chktex"},
  vim = {"vint", "cspell"},
  yaml = {"yamllint", "cspell"},
  zsh = {"shellcheck", "cspell"}
}

-- Set up autocmd to trigger linting
vim.api.nvim_exec([[
  autocmd BufWritePost <buffer> lua require('lint').try_lint()
  ]], true)

-- vim:foldenable:foldmethod=marker:foldlevel=0
