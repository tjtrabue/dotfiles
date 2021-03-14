-- Configuration for Neovim's native LSP functions.
-- Must source this file after the `nvim-lspconfig` plugin loads.

-- Private functions {{{

--- Execute the OS command `cmd` and return the result as a string.
local function os_cmd_to_string(cmd)
    local handle = io.popen(cmd)
    local result = handle:read("*a")
    handle:close()

    return result
end
-- }}}

-- elixir-ls {{{
require'lspconfig'.elixirls.setup{
    cmd = { os_cmd_to_string("command -v elixir-ls") };
}
-- }}}

-- lua-language-server {{{
local system_name
if vim.fn.has("mac") == 1 then
  system_name = "macOS"
elseif vim.fn.has("unix") == 1 then
  system_name = "Linux"
elseif vim.fn.has('win32') == 1 then
  system_name = "Windows"
else
  print("Unsupported system for sumneko")
end

-- set the path to the sumneko installation; if you previously installed via the now deprecated :LspInstall, use
local sumneko_binary = os_cmd_to_string("command -v lua-language-server")

require'lspconfig'.sumneko_lua.setup {
  cmd = {sumneko_binary};
  settings = {
    Lua = {
      runtime = {
        -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
        version = 'LuaJIT',
        -- Setup your lua path
        path = vim.split(package.path, ';'),
      },
      diagnostics = {
        -- Get the language server to recognize the `vim` global
        globals = {'vim'},
      },
      workspace = {
        -- Make the server aware of Neovim runtime files
        library = {
          [vim.fn.expand('$VIMRUNTIME/lua')] = true,
          [vim.fn.expand('$VIMRUNTIME/lua/vim/lsp')] = true,
        },
      },
    },
  },
}
-- }}}

-- vim-language-server {{{
require'lspconfig'.vimls.setup{}
-- }}}

-- vim:foldenable:foldmethod=marker:foldlevel=0
