-- Define formatter functions for easy reuse.

-- shfmt
local function formatter_shfmt()
  return {
    exe = "shfmt",
    args = {"-s", "-"},
    stdin = true
  }
end

-- beautysh
local function formatter_beautysh()
  return {
    exe = "beautysh",
    args = {"--indent-size", "2", "-"},
    stdin = true
  }
end

-- black
local function formatter_black()
  return {
    exe = "black",
    args = {"--stdin-filename", vim.api.nvim_buf_get_name(0), "-"},
    stdin = true
  }
end

-- clang-format
local function formatter_clang_format()
  return {
    exe = "clang-format",
    stdin = true
  }
end

-- prettier
local function formatter_prettier()
  return {
    exe = "prettier",
    args = {"--stdin-filepath", vim.api.nvim_buf_get_name(0)},
    stdin = true
  }
end

-- luafmt
local function formatter_luafmt()
  return {
    exe = "luafmt",
    args = {"--indent-count", 2, "--stdin"},
    stdin = true
  }
end

-- mix format
local function formatter_mix_format()
  return {
    exe = "mix",
    args = {"format", "-"},
    stdin = true
  }
end

-- perltidy
local function formatter_perltidy()
  return {
    exe = "perltidy",
    args = {"--standard-output"},
    stdin = true
  }
end

-- yapf
local function formatter_yapf()
  return {
    exe = "yapf",
    args = {"--parallel"},
    stdin = true
  }
end

-- rustfmt
local function formatter_rustfmt()
  return {
    exe = "rustfmt",
    args = {"--emit=stdout"},
    stdin = true
  }
end

-- sql-formatter
local function formatter_sql_formatter()
  return {
    exe = "sql-formatter",
    args = {
      "--language",
      "postgresql",
      "--indent",
      "2",
      "--uppercase",
      "--lines-between-queries",
      "2"
    },
    stdin = true
  }
end

-- sqlformat
local function formatter_sqlformat()
  return {
    exe = "sqlformat",
    args = {
      "--identifiers=lower",
      "--keywords=upper",
      "--indent_width=2",
      "-"
    },
    stdin = true
  }
end

-- tidy for XML
local function formatter_tidy_xml()
  return {
    exe = "tidy",
    args = {
      "-xml",
      "-utf8",
      "-bare",
      "-clean",
      "-indent",
      "-wrap",
      "80",
      "--vertical-space",
      "yes"
    },
    stdin = true
  }
end

-- xmllint
local function formatter_xmllint()
  return {
    exe = "xmllint",
    args = {"--format", "-"},
    stdin = true
  }
end

-- yamlfmt
local function formatter_yamlfmt()
  return {
    exe = "yamlfmt",
    args = {},
    stdin = true
  }
end

-- Assign formatters for each filetype
require("formatter").setup(
  {
    logging = false,
    filetype = {
      bash = {
        -- formatter_beautysh,
        formatter_shfmt
      },
      c = {
        formatter_clang_format
      },
      cpp = {
        formatter_clang_format
      },
      css = {
        formatter_prettier
      },
      elixir = {
        formatter_mix_format
      },
      elm = {
        formatter_prettier
      },
      graphql = {
        formatter_prettier
      },
      html = {
        formatter_prettier
      },
      javascript = {
        formatter_prettier
      },
      ["javascript.jsx"] = {
        formatter_prettier
      },
      javascriptreact = {
        formatter_prettier
      },
      json = {
        formatter_prettier
      },
      jsonc = {
        formatter_prettier
      },
      jsx = {
        formatter_prettier
      },
      less = {
        formatter_prettier
      },
      lua = {
        formatter_luafmt
      },
      markdown = {
        formatter_prettier
      },
      perl = {
        formatter_perltidy
      },
      python = {
        formatter_black
      },
      ruby = {
        formatter_prettier
      },
      rust = {
        formatter_rustfmt
      },
      sass = {
        formatter_prettier
      },
      scss = {
        formatter_prettier
      },
      sh = {
        -- formatter_beautysh,
        formatter_shfmt
      },
      sql = {
        formatter_sql_formatter
      },
      toml = {
        formatter_prettier
      },
      typescript = {
        formatter_prettier
      },
      xml = {
        -- formatter_xmllint,
        formatter_prettier
      },
      yaml = {
        -- formatter_yamlfmt,
        formatter_prettier
      }
    }
  }
)

vim.api.nvim_exec(
  [[
" Format whole buffer
nnoremap <silent> <leader>FF :Format<CR>

" Format the specified filetypes on save.
augroup FormatAutogroup
  autocmd!
  autocmd FileType bash autocmd BufWritePost <buffer> FormatWrite
  autocmd FileType c autocmd BufWritePost <buffer> FormatWrite
  autocmd FileType cpp autocmd BufWritePost <buffer> FormatWrite
  autocmd FileType css autocmd BufWritePost <buffer> FormatWrite
  autocmd FileType elixir autocmd BufWritePost <buffer> FormatWrite
  autocmd FileType elm autocmd BufWritePost <buffer> FormatWrite
  autocmd FileType graphql autocmd BufWritePost <buffer> FormatWrite
  autocmd FileType html autocmd BufWritePost <buffer> FormatWrite
  autocmd FileType javascript autocmd BufWritePost <buffer> FormatWrite
  autocmd FileType javascript.jsx autocmd BufWritePost <buffer> FormatWrite
  autocmd FileType javascriptreact autocmd BufWritePost <buffer> FormatWrite
  autocmd FileType json autocmd BufWritePost <buffer> FormatWrite
  autocmd FileType jsonc autocmd BufWritePost <buffer> FormatWrite
  autocmd FileType jsx autocmd BufWritePost <buffer> FormatWrite
  autocmd FileType less autocmd BufWritePost <buffer> FormatWrite
  autocmd FileType lua autocmd BufWritePost <buffer> FormatWrite
  autocmd FileType markdown autocmd BufWritePost <buffer> FormatWrite
  autocmd FileType perl autocmd BufWritePost <buffer> FormatWrite
  autocmd FileType python autocmd BufWritePost <buffer> FormatWrite
  autocmd FileType ruby autocmd BufWritePost <buffer> FormatWrite
  autocmd FileType rust autocmd BufWritePost <buffer> FormatWrite
  autocmd FileType sass autocmd BufWritePost <buffer> FormatWrite
  autocmd FileType scss autocmd BufWritePost <buffer> FormatWrite
  autocmd FileType sh autocmd BufWritePost <buffer> FormatWrite
  " autocmd FileType sql autocmd BufWritePost <buffer> FormatWrite
  autocmd FileType toml autocmd BufWritePost <buffer> FormatWrite
  autocmd FileType typescript autocmd BufWritePost <buffer> FormatWrite
  " autocmd FileType xml autocmd BufWritePost <buffer> FormatWrite
  autocmd FileType yaml autocmd BufWritePost <buffer> FormatWrite
augroup END
]],
  true
)

-- vim:foldenable:foldmethod=indent:foldnestmax=1
