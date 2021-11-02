-- Treesitter
-- An advanced, incremental alternative to syntax parsing for programming
-- languages.

require "nvim-treesitter.configs".setup {
  ensure_installed = "maintained", -- one of "all", "maintained" (parsers with maintainers), or a list of languages
  -- ignore_install = {"javascript"}, -- List of parsers to ignore installing
  highlight = {
    enable = true, -- false will disable the whole extension
    -- disable = {"c", "rust"}, -- list of language that will be disabled
    -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
    -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
    -- Using this option may slow down your editor, and you may see some duplicate highlights.
    -- Instead of true it can also be a list of languages
    additional_vim_regex_highlighting = true
  }
}

-- Add Org Mode module for Tree Sitter
local parser_config = require "nvim-treesitter.parsers".get_parser_configs()
parser_config.org = {
  install_info = {
    url = "https://github.com/milisims/tree-sitter-org",
    files = {"src/parser.c", "src/scanner.cc"}
  },
  filetype = "org"
}
