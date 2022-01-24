-- nvim-cmp is a super flexible and powerful completion plugin for Neovim.
-- It even completes in the command entry area.

local cmp = require "cmp"

-- Use VSCode-like pictograms
local lspkind = require "lspkind"

cmp.setup(
  {
    formatting = {
      -- Use VSCode-like pictograms in completions
      format = lspkind.cmp_format({with_text = false, maxwidth = 50})
    },
    snippet = {
      expand = function(args)
        -- Choose your snippets provider. vsnip will be awesome once it gets
        -- more snippets in the friendly-snippets repo.
        -- vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
        -- require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
        vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
        -- require'snippy'.expand_snippet(args.body) -- For `snippy` users.
      end
    },
    mapping = {
      ["<C-l>"] = cmp.mapping.confirm({select = true}),
      ["<Tab>"] = cmp.mapping(cmp.mapping.select_next_item(), {"i", "s"}),
      -- Use C-j and C-k to cycle forward or backward through completion
      -- candidates.
      ["<C-j>"] = cmp.mapping(cmp.mapping.select_next_item(), {"i", "c", "s"}),
      ["<C-k>"] = cmp.mapping(cmp.mapping.select_prev_item(), {"i", "c", "s"}),
      ["<C-d>"] = cmp.mapping(cmp.mapping.scroll_docs(-4), {"i", "c"}),
      ["<C-f>"] = cmp.mapping(cmp.mapping.scroll_docs(4), {"i", "c"}),
      ["<C-Space>"] = cmp.mapping(cmp.mapping.complete(), {"i", "c"}),
      ["<C-y>"] = cmp.config.disable, -- If you want to remove the default `<C-y>` mapping, You can specify `cmp.config.disable` value.
      ["<C-e>"] = cmp.mapping(
        {
          i = cmp.mapping.abort(),
          c = cmp.mapping.close()
        }
      ),
      ["<CR>"] = cmp.mapping.confirm({select = true})
    },
    sources = cmp.config.sources(
      {
        {name = "nvim_lsp"},
        -- {name = "vsnip"} -- For vsnip users.
        -- { name = 'luasnip' }, -- For luasnip users.
        {name = "ultisnips"} -- For ultisnips users.
        -- { name = 'snippy' }, -- For snippy users.
      },
      {
        {name = "buffer"}
      }
    )
  }
)

-- Use buffer source for `/`.
cmp.setup.cmdline(
  "/",
  {
    sources = {
      {name = "buffer"}
    }
  }
)

-- Use cmdline & path source for ':'.
cmp.setup.cmdline(
  ":",
  {
    sources = cmp.config.sources(
      {
        {name = "path"}
      },
      {
        {name = "cmdline"}
      }
    )
  }
)

-- vim:foldenable:foldmethod=marker:foldlevel=0
