require("conform").setup({
  formatters_by_ft = {
    awk = { "gawk" },
    c = { "clang-format" },
    clojure = { "cljstyle", "cljfmt", stop_after_first = true },
    cmake = { "cmake_format" },
    css = { "prettier" },
    erlang = { "erlfmt" },
    go = { "goimports", "gofmt" },
    html = { "prettier" },
    -- Conform will run the first available formatter
    javascript = { "prettierd", "prettier", stop_after_first = true },
    lua = { "stylua" },
    markdown = { "mdformat" },
    perl = { "perltidy" },
    -- Conform will run multiple formatters sequentially
    python = { "isort", "black" },
    r = { "styler" },
    ruby = { "rubyfmt" },
    -- You can customize some of the format options for the filetype (:help conform.format)
    rust = { "rustfmt", lsp_format = "fallback" },
    sh = { "shfmt" },
    swift = { "swift" },
    yaml = { "yamlfmt" },
  },

  -- If this is set, Conform will run the formatter on save.
  -- It will pass the table to conform.format().
  -- This can also be a function that returns the table
  format_on_save = {
    -- These options are recommended by the plugin's author
    lsp_format = "fallback",
    timeout_ms = 500,
  },
})
