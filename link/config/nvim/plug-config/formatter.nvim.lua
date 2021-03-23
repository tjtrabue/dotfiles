-- Configure formatters for each filetype.
require("formatter").setup(
  {
    logging = false,
    filetype = {
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
      rust = {
        -- Rustfmt
        function()
          return {
            exe = "rustfmt",
            args = {"--emit=stdout"},
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
  autocmd BufWritePost *.js,*.lua,*.md,*.mkd,*.pl,*.plx,*.rs FormatWrite
augroup END
]],
  true
)

-- vim:foldenable:foldmethod=indent:foldnestmax=1
