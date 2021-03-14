-- Configuration for Neovim's native LSP functions.
-- Must source this file after the `nvim-lspconfig` plugin loads.

-- Private functions {{{

--- Execute the OS command `cmd` and return the result as a string.
function osCmdToString(cmd)
    local handle = io.popen(cmd)
    local result = handle:read("*a")
    handle:close()

    return result
end
-- }}}

-- elixir-ls {{{
require'lspconfig'.elixirls.setup{
    cmd = { osCmdToString("command -v elixir-ls") };
}
-- }}}

-- vim-language-server {{{
require'lspconfig'.vimls.setup{}
-- }}}

-- vim:foldenable:foldmethod=marker:foldlevel=0
