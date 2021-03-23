-- Configure formatters for each filetype.
require("formatter").setup(
  {
    logging = false,
    filetype = {
      bash = {
        -- shfmt
        function()
          return {
            exe = "shfmt",
            -- args = {},
            stdin = true
          }
        end,
        -- beautysh
        function()
          return {
            exe = "beautysh",
            args = {"--indent-size", "2", "-"},
            stdin = true
          }
        end
      },
      c = {
        -- clang-format
        function()
          return {
            exe = "clang-format",
            stdin = true
          }
        end
      },
      cpp = {
        -- clang-format
        function()
          return {
            exe = "clang-format",
            stdin = true
          }
        end
      },
      javascript = {
        -- prettier
        function()
          return {
            exe = "prettier",
            args = {"--stdin-filepath", vim.api.nvim_buf_get_name(0)},
            stdin = true
          }
        end
      },
      lua = {
        -- luafmt
        function()
          return {
            exe = "luafmt",
            args = {"--indent-count", 2, "--stdin"},
            stdin = true
          }
        end
      },
      markdown = {
        -- prettier
        function()
          return {
            exe = "prettier",
            args = {"--stdin-filepath", vim.api.nvim_buf_get_name(0)},
            stdin = true
          }
        end
      },
      perl = {
        -- perltidy
        function()
          return {
            exe = "perltidy",
            args = {"--standard-output"},
            stdin = true
          }
        end
      },
      python = {
        -- yapf
        function()
          return {
            exe = "yapf",
            args = {"--parallel"},
            stdin = true
          }
        end
      },
      rust = {
        -- Rustfmt
        function()
          return {
            exe = "rustfmt",
            args = {"--emit=stdout"},
            stdin = true
          }
        end
      },
      sh = {
        -- shfmt
        function()
          return {
            exe = "shfmt",
            -- args = {},
            stdin = true
          }
        end,
        -- beautysh
        function()
          return {
            exe = "beautysh",
            args = {"--indent-size", "2", "-"},
            stdin = true
          }
        end
      }
    }
  }
)

-- Format the specified filetypes on save.
vim.api.nvim_exec(
  [[
augroup FormatAutogroup
  autocmd!
  autocmd FileType bash autocmd BufWritePre <buffer> FormatWrite
  autocmd FileType c autocmd BufWritePre <buffer> FormatWrite
  autocmd FileType cpp autocmd BufWritePre <buffer> FormatWrite
  autocmd FileType javascript autocmd BufWritePre <buffer> FormatWrite
  autocmd FileType lua autocmd BufWritePre <buffer> FormatWrite
  autocmd FileType markdown autocmd BufWritePre <buffer> FormatWrite
  autocmd FileType perl autocmd BufWritePre <buffer> FormatWrite
  autocmd FileType python autocmd BufWritePre <buffer> FormatWrite
  autocmd FileType rust autocmd BufWritePre <buffer> FormatWrite
  autocmd FileType sh autocmd BufWritePre <buffer> FormatWrite
augroup END
]],
  true
)

-- vim:foldenable:foldmethod=indent:foldnestmax=1
