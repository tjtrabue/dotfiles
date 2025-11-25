-- Configuration for Neovim's native LSP functions,.
-- Must source this file after the `nvim-lspconfig` plugin loads.

local fs = require("tjdot.fs")
local str = require("tjdot.str")

-- nvim-cmp completion capabilities for Neovim's LSP.
local capabilities = require("cmp_nvim_lsp").update_capabilities(
  vim.lsp.protocol.make_client_capabilities()
)

-- Function called when a buffer attaches to a language server.
local on_attach = function(client, bufnr)
  local function buf_set_keymap(...)
    vim.api.nvim_buf_set_keymap(bufnr, ...)
  end

  vim.api.nvim_set_option_value("omnifunc", "v:lua.vim.lsp.omnifunc", {})

  -- Mappings
  local opts = { noremap = true, silent = true }
  buf_set_keymap("n", "gD", "<Cmd>lua vim.lsp.buf.declaration()<CR>", opts)
  buf_set_keymap("n", "gd", "<Cmd>lua vim.lsp.buf.definition()<CR>", opts)
  buf_set_keymap("n", "K", "<Cmd>lua vim.lsp.buf.hover()<CR>", opts)
  buf_set_keymap("n", "gi", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)
  buf_set_keymap(
    "n",
    "<C-k>",
    "<cmd>lua vim.lsp.buf.signature_help()<CR>",
    opts
  )
  buf_set_keymap(
    "n",
    "<leader>lwa",
    "<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>",
    opts
  )
  buf_set_keymap(
    "n",
    "<leader>lwr",
    "<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>",
    opts
  )
  buf_set_keymap(
    "n",
    "<leader>wl",
    "<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>",
    opts
  )
  buf_set_keymap(
    "n",
    "<leader>lD",
    "<cmd>lua vim.lsp.buf.type_definition()<CR>",
    opts
  )
  buf_set_keymap("n", "<leader>lr", "<cmd>lua vim.lsp.buf.rename()<CR>", opts)
  buf_set_keymap("n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", opts)
  buf_set_keymap(
    "n",
    "<leader>le",
    "<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>",
    opts
  )
  buf_set_keymap("n", "[d", "<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>", opts)
  buf_set_keymap("n", "]d", "<cmd>lua vim.lsp.diagnostic.goto_next()<CR>", opts)
  buf_set_keymap(
    "n",
    "<leader>lq",
    "<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>",
    opts
  )
  buf_set_keymap(
    "n",
    "<leader>la",
    "<cmd>lua vim.lsp.buf.code_action()<CR>",
    opts
  )
  -- Set some keybinds conditional on server capabilities
  if client.server_capabilities.document_formatting then
    buf_set_keymap(
      "n",
      "<leader>lf",
      "<cmd>lua vim.lsp.buf.formatting()<CR>",
      opts
    )
  elseif client.server_capabilities.document_range_formatting then
    buf_set_keymap(
      "n",
      "<leader>lf",
      "<cmd>lua vim.lsp.buf.range_formatting()<CR>",
      opts
    )
  end

  -- Set autocommands conditional on server_capabilities
  if client.server_capabilities.document_highlight then
    vim.api.nvim_exec2(
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
      { output = false }
    )
  end
end

-- Language servers
-- bash-language-server
vim.lsp.config("bashls", {
  capabilities = capabilities,
  on_attach = on_attach,
})
vim.lsp.enable("bashls")

-- clangd
-- NOTE: Clang >= 9 is recommended! See this issue for more.
--
-- clangd relies on a JSON compilation database specified as
-- compile_commands.json or, for simpler projects, a compile_flags.txt.
-- For details on how to automatically generate one using CMake look here.

vim.lsp.config("clangd", {
  capabilities = capabilities,
  on_attach = on_attach,
})
vim.lsp.enable("clangd")

-- clojure-lsp
vim.lsp.config("clojure_lsp", {
  capabilities = capabilities,
  on_attach = on_attach,
})
vim.lsp.enable("clojure_lsp")

-- cmake-ls
vim.lsp.config("cmake", {
  capabilities = capabilities,
  on_attach = on_attach,
})
vim.lsp.enable("cmake")

-- cssls
-- Currently disabled in favor of tailwindcss
--[[ vim.lsp.config("cssls", {
  capabilities = capabilities,
  on_attach = on_attach
} ]]
-- cssmodules-language-server
vim.lsp.config("cssmodules_ls", {
  capabilities = capabilities,
  on_attach = on_attach,
})
vim.lsp.enable("cssmodules_ls")

-- dartls
vim.lsp.config("dartls", {
  capabilities = capabilities,
  on_attach = on_attach,
})
vim.lsp.enable("dartls")

-- dockerfile-ls
vim.lsp.config("dockerls", {
  capabilities = capabilities,
  on_attach = on_attach,
})
vim.lsp.enable("dockerls")

-- dot-language-server
vim.lsp.config("dotls", {
  capabilities = capabilities,
  on_attach = on_attach,
})
vim.lsp.enable("dotls")

-- efm-language-server
-- Not currently used because EFM requires a lot of setup, and is not terribly
-- useful.
--[[ vim.lsp.config("efm", {
  capabilities = capabilities,
  on_attach = on_attach
} ]]
-- elixir-ls
-- Currently using `lexical` instead.
--[[ local elixir_ls_binary = fs.os_cmd_to_string("command -v elixir-ls")
if not str.isempty(elixir_ls_binary) then
  vim.lsp.config("elixirls", {
    capabilities = capabilities,
    cmd = {elixir_ls_binary},
    on_attach = on_attach,
    settings = {
      elixirLS = {
        dialyzerEnabled = true,
        fetchDeps = true
      }
    }
  }
end ]]
-- lexical (newer Elixir LSP server)
local lexical_binary = fs.os_cmd_to_string("command -v start_lexical.sh")
if not str.isempty(lexical_binary) then
  vim.lsp.config("lexical", {
    capabilities = capabilities,
    cmd = { lexical_binary },
    on_attach = on_attach,
  })
  vim.lsp.enable("lexical")
end

-- emmet-ls (for HTML templating/snippet expansion)
vim.lsp.config("emmet_ls", {
  capabilities = capabilities,
  on_attach = on_attach,
})
vim.lsp.enable("emmet_ls")

-- erlang-ls
vim.lsp.config("erlangls", {
  capabilities = capabilities,
  on_attach = on_attach,
})
vim.lsp.enable("erlangls")

-- graphql-language-server
vim.lsp.config("graphql", {
  capabilities = capabilities,
  on_attach = on_attach,
})
vim.lsp.enable("graphql")

-- haskell-language-server
vim.lsp.config("hls", {
  capabilities = capabilities,
  on_attach = on_attach,
})
vim.lsp.enable("hls")

-- html-ls
--Enable (broadcasting) snippet capability for completion
local html_capabilities = require("cmp_nvim_lsp").update_capabilities(
  vim.lsp.protocol.make_client_capabilities()
)
html_capabilities.textDocument.completion.completionItem.snippetSupport = true

vim.lsp.config("html", {
  capabilities = html_capabilities,
  on_attach = on_attach,
})
vim.lsp.enable("html")

-- intelephense
vim.lsp.config("intelephense", {
  capabilities = capabilities,
  on_attach = on_attach,
})
vim.lsp.enable("intelephense")

-- jsonls
-- vscode-json-languageserver only provides range formatting. You can map a
-- command that applies range formatting to the entire document:
vim.lsp.config("jsonls", {
  capabilities = capabilities,
  commands = {
    Format = {
      function()
        vim.lsp.buf.range_formatting({}, { 0, 0 }, { vim.fn.line("$"), 0 })
      end,
    },
  },
  on_attach = on_attach,
})
vim.lsp.enable("jsonls")

-- kotlin-language-server
-- This server is currently awful. Just terrible. You should not use it if you
-- have any other options. Use IntelliJ IDEA, use Android Studio, use VSCode,
-- use anything else but this if you can. It is far too slow, and does not
-- integrate well with non-VSCode editors.
local kotlin_language_server_binary =
  fs.os_cmd_to_string("command -v kotlin-language-server")
if not str.isempty(kotlin_language_server_binary) then
  vim.lsp.config("kotlin_language_server", {
    capabilities = capabilities,
    cmd = { kotlin_language_server_binary },
    filetypes = { "kotlin" },
    on_attach = on_attach,
    root_markers = {
      "settings.gradle",
      "settings.gradle.kts",
      ".git",
    },
    settings = {
      -- Most of these settings are defaults, but for some reason it was necessary
      -- to specify them to get the kotlin-language-server to work.
      kotlin = {
        compiler = {
          jvm = {
            target = "default",
          },
        },
        completion = {
          snippets = {
            enabled = true,
          },
        },
        debounceTime = 250,
        debugAdapter = {
          path = "",
        },
        externalSources = {
          autoConvertToKotlin = true,
          useKlsScheme = true,
        },
        indexing = {
          enabled = true,
        },
        languageServer = {
          debugAttach = {
            autoSuspend = false,
            enabled = false,
            port = 5005,
          },
          enabled = true,
          path = kotlin_language_server_binary,
          port = 0,
          transport = "stdio",
        },
        linting = {
          debounceTime = 250,
        },
        snippetsEnabled = true,
        trace = {
          server = "off",
        },
      },
    },
    single_file_support = true,
  })
  vim.lsp.enable("kotlin_language_server")
end

-- lua-language-server
vim.lsp.config("lua_ls", {
  capabilities = capabilities,
  on_attach = on_attach,
})
vim.lsp.enable("lua_ls")

-- Marksman Markdown LSP {{{
vim.lsp.config("marksman", {
  capabilities = capabilities,
  cmd = { "marksman", "server" },
  filetypes = { "markdown" },
  on_attach = on_attach,
  root_markers = { ".git", ".marksman.toml" },
})
vim.lsp.enable("marksman")
-- }}}

-- Perl-Language-Server
-- To use the language server, ensure that you have Perl::LanguageServer
-- installed and perl command is on your path.
vim.lsp.config("perlls", {
  capabilities = capabilities,
  on_attach = on_attach,
})
vim.lsp.enable("perlls")

-- pyright
vim.lsp.config("pyright", {
  capabilities = capabilities,
  on_attach = on_attach,
})
vim.lsp.enable("pyright")

-- rls (Rust)
vim.lsp.config("rls", {
  capabilities = capabilities,
  -- Use nightly build
  cmd = { "rustup", "run", "nightly", "rls" },
  on_attach = on_attach,
})
vim.lsp.enable("rls")

-- sqlls (SQL LanguageServer written in Node.js)
--[[ local sql_ls_bin = fs.os_cmd_to_string("command -v sql-language-server")

vim.lsp.config("sqlls".setup {
  capabilities = capabilities,
  cmd = {sql_ls_bin},
  on_attach = on_attach
} ]]
-- sqls (SQL LanguageServer written in Go)
-- To install sqls, run this command:
--   go get github.com/lighttiger2505/sqls
local sqls_binary = fs.os_cmd_to_string("command -v sqls")
local sqls_config_file = os.getenv("HOME") .. "/.config/sqls/config.yml"

if not str.isempty(sqls_binary) then
  vim.lsp.config("sqls", {
    capabilities = capabilities,
    cmd = { sqls_binary, "-config", sqls_config_file },
    on_attach = on_attach,
  })
  vim.lsp.enable("sqls")
end

-- tailwind-css
vim.lsp.config("tailwindcss", {
  capabilities = capabilities,
  on_attach = on_attach,
})
vim.lsp.enable("tailwindcss")

-- terraform-lsp
vim.lsp.config("terraform_lsp", {
  capabilities = capabilities,
  on_attach = on_attach,
})
vim.lsp.enable("terraform_lsp")

-- texlab (LaTeX)
vim.lsp.config("texlab", {
  capabilities = capabilities,
  on_attach = on_attach,
})
vim.lsp.enable("texlab")

-- typescript-language-server
vim.lsp.config("tsserver", {
  capabilities = capabilities,
  on_attach = on_attach,
})
vim.lsp.enable("tsserver")

-- vim-language-server
vim.lsp.config("vimls", {
  capabilities = capabilities,
  on_attach = on_attach,
})
vim.lsp.enable("vimls")

-- yaml-language-server
vim.lsp.config("yamlls", {
  capabilities = capabilities,
  on_attach = on_attach,
})
vim.lsp.enable("yamlls")
