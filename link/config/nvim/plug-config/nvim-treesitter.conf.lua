local parser_config = require "nvim-treesitter.parsers".get_parser_configs()
parser_config.org = {
  install_info = {
    url = "milisims/tree-sitter-org",
    files = {"src/parser.c", "src/scanner.cc"}
  },
  filetype = "org"
}
