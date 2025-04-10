-- Treesitter
-- An advanced, incremental alternative to syntax parsing for programming
-- languages.

require "nvim-treesitter.configs".setup {
  -- A list of parser names, or "all" (the listed parsers MUST always be
  -- installed)
  ensure_installed = "all",
  -- Whether to install parsers synchronously.
  sync_install = false,
  -- Automatically install missing parsers when entering buffer.
  -- Recommendation: set to false if you don't have `tree-sitter` CLI installed
  -- locally.
  auto_install = true,
  --List of parsers to ignore installing (or "all")
  ignore_install = {"org"},
  highlight = {
    enable = true, -- false will disable the whole extension
    -- disable = {"org"}, -- list of language that will be disabled
    -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
    -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
    -- Using this option may slow down your editor, and you may see some duplicate highlights.
    -- Instead of true it can also be a list of languages
    additional_vim_regex_highlighting = true
  }
}
