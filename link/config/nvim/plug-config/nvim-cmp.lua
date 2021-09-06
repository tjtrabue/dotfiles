local cmp = require "cmp"

cmp.setup {
  snippet = {
    expand = function(args)
      vim.fn["vsnip#anonymous"](args.body)
    end
  },
  mapping = {
    ["<C-l>"] = cmp.mapping.confirm({select = true}),
    ["<Tab>"] = cmp.mapping(cmp.mapping.select_next_item(), {"i", "s"}),
    -- Use C-j and C-k to cycle forward or backward through completion
    -- candidates.
    ["<C-j>"] = cmp.mapping(cmp.mapping.select_next_item(), {"i", "s"}),
    ["<C-k>"] = cmp.mapping(cmp.mapping.select_prev_item(), {"i", "s"})
  }
}

-- vim:foldenable:foldmethod=marker:foldlevel=0
