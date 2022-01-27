-- Configuration for Neovim's native LSP functions.
-- Must source this file after the `nvim-lspconfig` plugin loads.

local lspconfig = require("lspconfig")
local util = require("lspconfig.util")

-- Private function

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

-- nvim-cmp completion capabilities for Neovim's LSP.
local capabilities = require("cmp_nvim_lsp").update_capabilities(vim.lsp.protocol.make_client_capabilities())

-- Function called when a buffer attaches to a language server.
local on_attach = function(client, bufnr)
  local function buf_set_keymap(...)
    vim.api.nvim_buf_set_keymap(bufnr, ...)
  end
  local function buf_set_option(...)
    vim.api.nvim_buf_set_option(bufnr, ...)
  end

  -- Aerial code browser config.
  require("aerial").on_attach(client, bufnr)

  buf_set_option("omnifunc", "v:lua.vim.lsp.omnifunc")

  -- Mappings
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
  buf_set_keymap("n", "<leader>la", "<cmd>lua vim.lsp.buf.code_action()<CR>", opts)
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

-- Language servers
-- bash-language-server
lspconfig.bashls.setup {
  capabilities = capabilities,
  on_attach = on_attach
}

-- clangd
-- NOTE: Clang >= 9 is recommended! See this issue for more.
--
-- clangd relies on a JSON compilation database specified as
-- compile_commands.json or, for simpler projects, a compile_flags.txt.
-- For details on how to automatically generate one using CMake look here.

lspconfig.clangd.setup {
  capabilities = capabilities,
  on_attach = on_attach
}

-- clojure-lsp
lspconfig.clojure_lsp.setup {
  capabilities = capabilities,
  on_attach = on_attach
}

-- cmake-ls
lspconfig.cmake.setup {
  capabilities = capabilities,
  on_attach = on_attach
}

-- cssls
-- Currently disabled in favor of tailwindcss
--[[ lspconfig.cssls.setup {
  capabilities = capabilities,
  on_attach = on_attach
} ]]
-- cssmodules-language-server
lspconfig.cssmodules_ls.setup {
  capabilities = capabilities,
  on_attach = on_attach
}

-- dartls
lspconfig.dartls.setup {
  capabilities = capabilities,
  on_attach = on_attach
}

-- dockerfile-ls
lspconfig.dockerls.setup {
  capabilities = capabilities,
  on_attach = on_attach
}

-- dot-language-server
lspconfig.dotls.setup {
  capabilities = capabilities,
  on_attach = on_attach
}

-- efm-language-server
-- Not currently used because EFM requires a lot of setup, and is not terribly
-- useful.
--[[ lspconfig.efm.setup {
  capabilities = capabilities,
  on_attach = on_attach
} ]]
-- elixir-ls
local elixir_ls_bin = os_cmd_to_string("command -v elixir-ls")
lspconfig.elixirls.setup {
  capabilities = capabilities,
  cmd = {elixir_ls_bin},
  on_attach = on_attach
}

-- emmet-ls (for HTML templating/snippet expansion)
lspconfig.emmet_ls.setup {
  capabilities = capabilities,
  on_attach = on_attach
}

-- erlang-ls
lspconfig.erlangls.setup {
  capabilities = capabilities,
  on_attach = on_attach
}

-- graphql-language-server
lspconfig.graphql.setup {
  capabilities = capabilities,
  on_attach = on_attach
}

-- haskell-language-server
lspconfig.hls.setup {
  capabilities = capabilities,
  on_attach = on_attach
}

-- html-ls
--Enable (broadcasting) snippet capability for completion
local html_capabilities = require("cmp_nvim_lsp").update_capabilities(vim.lsp.protocol.make_client_capabilities())
html_capabilities.textDocument.completion.completionItem.snippetSupport = true
lspconfig.html.setup {
  capabilities = html_capabilities,
  on_attach = on_attach
}

-- intelephense
lspconfig.intelephense.setup {
  capabilities = capabilities,
  on_attach = on_attach
}

-- jsonls
-- vscode-json-languageserver only provides range formatting. You can map a
-- command that applies range formatting to the entire document:
lspconfig.jsonls.setup {
  capabilities = capabilities,
  commands = {
    Format = {
      function()
        vim.lsp.buf.range_formatting({}, {0, 0}, {vim.fn.line("$"), 0})
      end
    }
  },
  on_attach = on_attach
}

-- kotlin_language_server
local kotlin_language_server_binary = os_cmd_to_string("command -v kotlin-language-server")
lspconfig.kotlin_language_server.setup {
  capabilities = capabilities,
  cmd = {kotlin_language_server_binary},
  filetypes = {"kotlin"},
  on_attach = on_attach,
  root_dir = util.root_pattern("settings.gradle", "settings.gradle.kts", ".git"),
  settings = {
    -- Most of these settings are defaults, but for some reason it was necessary
    -- to specify them to get the kotlin-language-server to work.
    kotlin = {
      compiler = {
        jvm = {
          target = "default"
        }
      },
      completion = {
        snippets = {
          enabled = true
        }
      },
      debounceTime = 250,
      debugAdapter = {
        path = ""
      },
      externalSources = {
        autoConvertToKotlin = true,
        useKlsScheme = true
      },
      indexing = {
        enabled = true
      },
      languageServer = {
        debugAttach = {
          autoSuspend = false,
          enabled = false,
          port = 5005
        },
        enabled = true,
        path = kotlin_language_server_binary,
        port = 0,
        transport = "stdio"
      },
      linting = {
        debounceTime = 250
      },
      snippetsEnabled = true,
      trace = {
        server = "off"
      }
    }
  },
  single_file_support = true
}

-- lua-language-server

local sumneko_binary = os_cmd_to_string("command -v lua-language-server")
local runtime_path = vim.split(package.path, ";")
table.insert(runtime_path, "lua/?.lua")
table.insert(runtime_path, "lua/?/init.lua")
lspconfig.sumneko_lua.setup {
  capabilities = capabilities,
  -- cmd = {sumneko_binary, "-E", sumneko_root_path .. "/main.lua"},
  cmd = {sumneko_binary},
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
      }
    }
  },
  on_attach = on_attach
}

-- Perl-Language-Server
-- To use the language server, ensure that you have Perl::LanguageServer
-- installed and perl command is on your path.
lspconfig.perlls.setup {
  capabilities = capabilities,
  on_attach = on_attach
}

-- pyright
lspconfig.pyright.setup {
  capabilities = capabilities,
  on_attach = on_attach
}

-- rls (Rust)
lspconfig.rls.setup {
  capabilities = capabilities,
  -- Use nightly build
  cmd = {"rustup", "run", "nightly", "rls"},
  on_attach = on_attach
}

-- sqlls (SQL LanguageServer written in Node.js)
--[[ local sql_ls_bin = os_cmd_to_string("command -v sql-language-server")

lspconfig.sqlls.setup {
  capabilities = capabilities,
  cmd = {sql_ls_bin},
  on_attach = on_attach
} ]]
-- sqls (SQL LanguageServer written in Go)
-- To install sqls, run this command:
--   go get github.com/lighttiger2505/sqls
local sqls_cmd = os_cmd_to_string("command -v sqls")
local sqls_config_file = os.getenv("HOME") .. "/.config/sqls/config.yml"

lspconfig.sqls.setup {
  capabilities = capabilities,
  cmd = {sqls_cmd, "-config", sqls_config_file},
  on_attach = on_attach
}

-- tailwind-css
lspconfig.tailwindcss.setup {
  capabilities = capabilities,
  on_attach = on_attach
}

-- terraform-lsp
lspconfig.terraform_lsp.setup {
  capabilities = capabilities,
  on_attach = on_attach
}

-- texlab (LaTeX)
lspconfig.texlab.setup {
  capabilities = capabilities,
  on_attach = on_attach
}

-- typescript-language-server
lspconfig.tsserver.setup {
  capabilities = capabilities,
  on_attach = on_attach
}

-- vim-language-server
lspconfig.vimls.setup {
  capabilities = capabilities,
  on_attach = on_attach
}

-- yaml-language-server
lspconfig.yamlls.setup {
  capabilities = capabilities,
  on_attach = on_attach
}
