-- Configuration for Neovim's native LSP functions.
-- Must source this file after the `nvim-lspconfig` plugin loads.

-- Private functions {{{

--- Execute the OS command `cmd` and return the result as a string.
local function os_cmd_to_string(cmd)
    -- The output string to return
    local str = ""
    -- get a temporary file name
    local tmp = os.tmpname()

    -- execute a command
    os.execute(cmd .. " > " .. tmp)

    -- display output
    for line in io.lines(tmp) do
        if str ~= "" then
            str = str .. "\n"
        end
        str = str .. line
    end

    -- remove temporary file
    os.remove(tmp)

    return str
end
-- }}}

-- bash-language-server {{{
require'lspconfig'.bashls.setup{}
-- }}}

-- clangd {{{
-- NOTE: Clang >= 9 is recommended! See this issue for more.
--
-- clangd relies on a JSON compilation database specified as
-- compile_commands.json or, for simpler projects, a compile_flags.txt.
-- For details on how to automatically generate one using CMake look here.

require'lspconfig'.clangd.setup{}
--- }}}

-- cmake-ls {{{
require'lspconfig'.cmake.setup{}
-- }}}

-- vscode-css-languageserver {{{
require'lspconfig'.cssls.setup{}
-- }}}

-- dockerfile-ls {{{
require'lspconfig'.dockerls.setup{}
-- }}}

-- efm-language-server {{{
require'lspconfig'.efm.setup{}
-- }}}

-- elixir-ls {{{
local elixir_ls_bin = os_cmd_to_string("command -v elixir-ls")

require'lspconfig'.elixirls.setup{
    cmd = { elixir_ls_bin }
}
-- }}}
--
-- erlang-ls {{{
require'lspconfig'.erlangls.setup{}
-- }}}

-- graphql-language-server {{{
require'lspconfig'.graphql.setup{}
-- }}}

-- haskell-language-server {{{
require'lspconfig'.hls.setup{}
-- }}}

-- vscode-html-language-server {{{
--Enable (broadcasting) snippet capability for completion
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true

require'lspconfig'.html.setup {
  capabilities = capabilities,
}
-- }}}

-- intelephense {{{
require'lspconfig'.intelephense.setup{}
-- }}}

-- vscode-json-language-server {{{

-- vscode-json-languageserver only provides range formatting. You can map a
-- command that applies range formatting to the entire document:
require'lspconfig'.jsonls.setup {
    commands = {
      Format = {
        function()
          vim.lsp.buf.range_formatting({},{0,0},{vim.fn.line("$"),0})
        end
      }
    }
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

-- Perl-Language-Server {{{

-- To use the language server, ensure that you have Perl::LanguageServer
-- installed and perl command is on your path.
require'lspconfig'.perlls.setup{}
-- }}}

-- python-language-server {{{
require'lspconfig'.pyls.setup{}
-- }}}

-- rls (Rust) {{{
require'lspconfig'.rls.setup{
    -- Use nightly build
    cmd = {"rustup", "run", "nightly", "rls"}
}
-- }}}

-- sqlls (SQL) {{{
--
local sql_ls_bin = os_cmd_to_string("command -v sql-language-server")

require'lspconfig'.sqlls.setup{
    cmd = {sql_ls_bin}
}
-- }}}

-- texlab (LaTeX) {{{
require'lspconfig'.texlab.setup{}
-- }}}

-- vim-language-server {{{
require'lspconfig'.vimls.setup{}
-- }}}

-- yaml-language-server {{{
require'lspconfig'.yamlls.setup{}
-- }}}

-- vim:foldenable:foldmethod=marker:foldlevel=0
