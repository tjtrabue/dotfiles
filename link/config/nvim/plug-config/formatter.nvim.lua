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
      javascript = {
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
        formatter_yapf
      },
      rust = {
        formatter_rustfmt
      },
      sh = {
        -- formatter_beautysh,
        formatter_shfmt
      },
      sql = {
        formatter_sql_formatter
      },
      xml = {
        formatter_xmllint
      },
      yaml = {
        formatter_yamlfmt
      }
    }
  }
)

-- Format the specified filetypes on save.
vim.api.nvim_exec(
  [[
augroup FormatAutogroup
  autocmd!
  autocmd FileType bash autocmd BufWritePost <buffer> FormatWrite
  autocmd FileType c autocmd BufWritePost <buffer> FormatWrite
  autocmd FileType cpp autocmd BufWritePost <buffer> FormatWrite
  autocmd FileType javascript autocmd BufWritePost <buffer> FormatWrite
  autocmd FileType lua autocmd BufWritePost <buffer> FormatWrite
  autocmd FileType markdown autocmd BufWritePost <buffer> FormatWrite
  autocmd FileType perl autocmd BufWritePost <buffer> FormatWrite
  autocmd FileType python autocmd BufWritePost <buffer> FormatWrite
  autocmd FileType rust autocmd BufWritePost <buffer> FormatWrite
  autocmd FileType sh autocmd BufWritePost <buffer> FormatWrite
  autocmd FileType sql autocmd BufWritePost <buffer> FormatWrite
  " autocmd FileType xml autocmd BufWritePost <buffer> FormatWrite
  autocmd FileType yaml autocmd BufWritePost <buffer> FormatWrite
augroup END
]],
  true
)

-- vim:foldenable:foldmethod=indent:foldnestmax=1
