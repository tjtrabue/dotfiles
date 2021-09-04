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

-- Custom configuration {{{

local nvim_lsp = require("lspconfig")
-- Function called when a buffer attaches to a language server.
local on_attach = function(client, bufnr)
  local function buf_set_keymap(...)
    vim.api.nvim_buf_set_keymap(bufnr, ...)
  end
  local function buf_set_option(...)
    vim.api.nvim_buf_set_option(bufnr, ...)
  end

  -- Pull in the completion-nvim completion backend, which is quite a bit faster
  -- than Neovim's default completion.
  require("completion").on_attach()

  buf_set_option("omnifunc", "v:lua.vim.lsp.omnifunc")

  -- Mappings.
  local opts = {noremap = true, silent = true}
  buf_set_keymap("n", "gD", "<Cmd>lua vim.lsp.buf.declaration()<CR>", opts)
  buf_set_keymap("n", "gd", "<Cmd>lua vim.lsp.buf.definition()<CR>", opts)
  buf_set_keymap("n", "K", "<Cmd>lua vim.lsp.buf.hover()<CR>", opts)
  buf_set_keymap("n", "gi", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)
  buf_set_keymap("n", "<C-k>", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)
  buf_set_keymap("n", "<leader>lwa", "<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>", opts)
  buf_set_keymap("n", "<leader>lwr", "<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>", opts)
  buf_set_keymap("n", "<leader>wl", "<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>", opts)
  buf_set_keymap("n", "<leader>lD", "<cmd>lua vim.lsp.buf.type_definition()<CR>", opts)
  buf_set_keymap("n", "<leader>lr", "<cmd>lua vim.lsp.buf.rename()<CR>", opts)
  buf_set_keymap("n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", opts)
  buf_set_keymap("n", "<leader>le", "<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>", opts)
  buf_set_keymap("n", "[d", "<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>", opts)
  buf_set_keymap("n", "]d", "<cmd>lua vim.lsp.diagnostic.goto_next()<CR>", opts)
  buf_set_keymap("n", "<leader>lq", "<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>", opts)

  -- Set some keybinds conditional on server capabilities
  if client.resolved_capabilities.document_formatting then
    buf_set_keymap("n", "<leader>lf", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)
  elseif client.resolved_capabilities.document_range_formatting then
    buf_set_keymap("n", "<leader>lf", "<cmd>lua vim.lsp.buf.range_formatting()<CR>", opts)
  end

  -- Set autocommands conditional on server_capabilities
  if client.resolved_capabilities.document_highlight then
    vim.api.nvim_exec(
      [[
        hi LspReferenceRead cterm=bold ctermbg=red guibg=LightYellow
        hi LspReferenceText cterm=bold ctermbg=red guibg=LightYellow
        hi LspReferenceWrite cterm=bold ctermbg=red guibg=LightYellow
        augroup lsp_document_highlight
        autocmd! * <buffer>
        autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
        autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
        augroup END
        ]],
      false
    )
  end
end
-- }}}

-- Language servers {{{
-- bash-language-server {{{
require "lspconfig".bashls.setup {
  on_attach = on_attach
}
-- }}}

-- clangd {{{
-- NOTE: Clang >= 9 is recommended! See this issue for more.
--
-- clangd relies on a JSON compilation database specified as
-- compile_commands.json or, for simpler projects, a compile_flags.txt.
-- For details on how to automatically generate one using CMake look here.

require "lspconfig".clangd.setup {
  on_attach = on_attach
}
--- }}}

-- cmake-ls {{{
require "lspconfig".cmake.setup {
  on_attach = on_attach
}
-- }}}

-- vscode-css-languageserver {{{
require "lspconfig".cssls.setup {
  on_attach = on_attach
}
-- }}}

-- dockerfile-ls {{{
require "lspconfig".dockerls.setup {
  on_attach = on_attach
}
-- }}}

-- efm-language-server {{{
-- Not currently used because EFM requires a lot of setup, and is not terribly
-- useful.
--[[ require "lspconfig".efm.setup {
  on_attach = on_attach
} ]]
-- }}}

-- elixir-ls {{{
local elixir_ls_bin = os_cmd_to_string("command -v elixir-ls")

require "lspconfig".elixirls.setup {
  cmd = {elixir_ls_bin},
  on_attach = on_attach
}
-- }}}
--
-- erlang-ls {{{
require "lspconfig".erlangls.setup {
  on_attach = on_attach
}
-- }}}

-- graphql-language-server {{{
require "lspconfig".graphql.setup {
  on_attach = on_attach
}
-- }}}

-- haskell-language-server {{{
require "lspconfig".hls.setup {
  on_attach = on_attach
}
-- }}}

-- vscode-html-language-server {{{
--Enable (broadcasting) snippet capability for completion
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true

require "lspconfig".html.setup {
  capabilities = capabilities,
  on_attach = on_attach
}
-- }}}

-- intelephense {{{
require "lspconfig".intelephense.setup {
  on_attach = on_attach
}
-- }}}

-- vscode-json-language-server {{{

-- vscode-json-languageserver only provides range formatting. You can map a
-- command that applies range formatting to the entire document:
require "lspconfig".jsonls.setup {
  commands = {
    Format = {
      function()
        vim.lsp.buf.range_formatting({}, {0, 0}, {vim.fn.line("$"), 0})
      end
    }
  },
  on_attach = on_attach
}
-- }}}

-- lua-language-server {{{
local system_name
if vim.fn.has("mac") == 1 then
  system_name = "macOS"
elseif vim.fn.has("unix") == 1 then
  system_name = "Linux"
elseif vim.fn.has("win32") == 1 then
  system_name = "Windows"
else
  print("Unsupported system for sumneko")
end

-- Set the path to the sumneko lua-language-server installation.
-- Use the "install_lua_language_server" function to install this LSP to the
-- standard location this configuration expects.
local sumneko_root_path = os.getenv("WS") .. "/lua-language-server"
local sumneko_binary = sumneko_root_path .. "/bin/" .. system_name .. "/lua-language-server"

local runtime_path = vim.split(package.path, ";")
table.insert(runtime_path, "lua/?.lua")
table.insert(runtime_path, "lua/?/init.lua")

require "lspconfig".sumneko_lua.setup {
  cmd = {sumneko_binary, "-E", sumneko_root_path .. "/main.lua"},
  settings = {
    Lua = {
      runtime = {
        -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
        version = "LuaJIT",
        -- Setup your lua path
        path = runtime_path
      },
      diagnostics = {
        -- Get the language server to recognize the `vim` global
        globals = {"vim"}
      },
      workspace = {
        -- Make the server aware of Neovim runtime files
        library = vim.api.nvim_get_runtime_file("", true),
        -- Neovim does not currently support third party tools
        checkThirdParty = false
      },
      -- Do not send telemetry data containing a randomized but unique identifier
      telemetry = {
        enable = false
      },
      -- Enable code hints
      hint = {
        enable = true
      }
    }
  },
  on_attach = on_attach
}
-- }}}

-- Perl-Language-Server {{{

-- To use the language server, ensure that you have Perl::LanguageServer
-- installed and perl command is on your path.
require "lspconfig".perlls.setup {
  on_attach = on_attach
}
-- }}}

-- pyright {{{
require "lspconfig".pyright.setup {
  on_attach = on_attach
}
-- }}}

-- rls (Rust) {{{
require "lspconfig".rls.setup {
  -- Use nightly build
  cmd = {"rustup", "run", "nightly", "rls"},
  on_attach = on_attach
}
-- }}}

-- sqlls (SQL) {{{
--
local sql_ls_bin = os_cmd_to_string("command -v sql-language-server")

require "lspconfig".sqlls.setup {
  cmd = {sql_ls_bin},
  on_attach = on_attach
}
-- }}}

-- texlab (LaTeX) {{{
require "lspconfig".texlab.setup {
  on_attach = on_attach
}
-- }}}

-- vim-language-server {{{
require "lspconfig".vimls.setup {
  on_attach = on_attach
}
-- }}}

-- yaml-language-server {{{
require "lspconfig".yamlls.setup {
  on_attach = on_attach
}
-- }}}
-- }}}

-- vim:foldenable:foldmethod=marker:foldlevel=0
