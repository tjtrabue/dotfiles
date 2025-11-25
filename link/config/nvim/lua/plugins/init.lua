-- [x] Remove folding comments
-- [x] Fix overall table syntax
-- [x] Fix dependencies syntax
-- [x] Fix config syntax
return {
  {
    "dhananjaylatkar/cscope_maps.nvim",
    after = "which-key.nvim",
  },

  { "jparise/vim-graphql" },

  { "edkolev/tmuxline.vim" },

  {
    "hoob3rt/lualine.nvim",
    dependencies = {
      "nvim-tree/nvim-web-devicons",
    },
  },

  { "romgrk/barbar.nvim" },

  { "rafi/awesome-vim-colorschemes" },
  { "rafamadriz/neon" },

  -- Highlight hex colorcodes and names in their respective colors.
  { "norcalli/nvim-colorizer.lua" },

  -- Adds fuzzy searching and other fun stuff to quickfix buffer.
  { "kevinhwang91/nvim-bqf" },

  { "kyazdani42/nvim-web-devicons" },

  { "shougo/neomru.vim" },

  { "tpope/vim-fugitive" },
  -- Newer and more universal replacement for gitgutter
  { "mhinz/vim-signify" },
  { "kdheepak/lazygit.nvim" },
  {
    "NeogitOrg/neogit",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope.nvim",
    },
  },
  { "f-person/git-blame.nvim" },

  { "SirVer/ultisnips" },
  { "honza/vim-snippets" },
  -- LSP snippets from VSCode
  { "hrsh7th/vim-vsnip" },
  { "hrsh7th/vim-vsnip-integ" },
  -- Pre-defined snippets for vim-vsnip.
  { "rafamadriz/friendly-snippets" },

  -- Multiple cursor support
  { "mg979/vim-visual-multi" },

  { "tpope/vim-bundler" },

  { "tpope/vim-surround" },

  { "godlygeek/tabular" },

  { "Shougo/unite.vim" },

  { "tpope/vim-unimpaired" },

  { "tpope/vim-vinegar" },

  { "tpope/vim-eunuch" },

  {
    "mbbill/undotree",
    config = function()
      vim.g.undotree_SetFocusWhenToggle = 1
    end,
  },

  -- Better completion engine using Lua.
  {
    "hrsh7th/nvim-cmp",
    dependencies = {
      "hrsh7th/vim-vsnip",
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-path",
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-cmdline",
    },
  },

  { "sakhnik/nvim-gdb" },

  { "plasticboy/vim-markdown" },
  -- NOTE: Requires Node.js and Yarn to work!
  {
    "iamcco/markdown-preview.nvim",
    run = function()
      vim.fn["mkdp#util#install"]()
    end,
  },

  { "cespare/vim-toml" },

  { "vim-scripts/c.vim" },

  { "hail2u/vim-css3-syntax" },
  { "ap/vim-css-color" },
  { "groenewege/vim-less" },
  { "dart-lang/dart-vim-plugin" },
  { "mxw/vim-jsx" },
  { "mattn/emmet-vim" },

  { "tpope/vim-rails" },

  { "vim-perl/vim-perl" },

  { "neovimhaskell/haskell-vim" },

  { "udalov/kotlin-vim" },

  { "lervag/vimtex" },
  { "xuhdev/vim-latex-live-preview" },

  { "tpope/vim-repeat" },
  { "wellle/targets.vim" },
  { "easymotion/vim-easymotion" },

  { "jiangmiao/auto-pairs" },
  { "tpope/vim-endwise" },

  { "lukas-reineke/indent-blankline.nvim" },
  { "nvim-lua/popup.nvim" },
  { "kevinhwang91/nvim-hlslens" },
  { "liuchengxu/vim-which-key" },

  { "mhinz/vim-startify" },

  { "majutsushi/tagbar" },

  {
    "neovim/nvim-lspconfig",
  },

  -- Better defaults for nvim-lsp.
  {
    "RishabhRD/nvim-lsputils",
    dependencies = {
      "RishabhRD/popfix",
    },
  },
  -- Patches colorschemes that don't support LSP client syntax for
  -- diagnostics.
  { "folke/lsp-colors.nvim" },
  -- Aerial is a code browser sidebar powered by nvim-lsp.
  { "stevearc/aerial.nvim" },
  -- Pretty diagnostic list for Neovim
  {
    "folke/trouble.nvim",
    dependencies = {
      "kyazdani42/nvim-web-devicons",
    },
    config = function()
      require("trouble").setup()
    end,
  },
  -- Full integration between nvim-lsp and Eclipse's JDTLS Java server.
  { "mfussenegger/nvim-jdtls" },
  -- Extra LSP configuration.
  -- Integrates LSP features with statusline.
  { "nvim-lua/lsp-status.nvim" },
  -- Show lightbulb icon in gutter when a code action is available.
  {
    "kosayoda/nvim-lightbulb",
    config = function()
      vim.cmd(
        "autocmd CursorHold,CursorHoldI * lua require'nvim-lightbulb'.update_lightbulb()"
      )
    end,
  },
  -- Make LSP client use FZF to navigate code
  {
    "ojroques/nvim-lspfuzzy",
    dependencies = {
      "junegunn/fzf",
      "junegunn/fzf.vim", -- to enable preview (optional)
    },
  },
  -- Floating window provider
  {
    "ray-x/guihua.lua",
    run = "cd lua/fzy && make",
  },
  -- Support signatures in auto-completion.
  {
    "ray-x/lsp_signature.nvim",
    config = function()
      require("lsp_signature").on_attach()
    end,
  },
  -- Use VSCode-like pictograms in auto-completion.
  { "onsails/lspkind-nvim" },

  -- Linting tool that complements Neovim's built-in LSP client.
  { "mfussenegger/nvim-lint" },

  -- Debugging
  { "mfussenegger/nvim-dap" },
  { "puremourning/vimspector" },

  { "chaoren/vim-wordmotion" },
  { "justinmk/vim-sneak" },

  { "junegunn/vim-easy-align" },

  { "nvim-lua/plenary.nvim" },

  { "nvim-telescope/telescope.nvim" },
  { "nvim-telescope/telescope-fzy-native.nvim" },

  { "sheerun/vim-polyglot" },
  {
    "nvim-treesitter/nvim-treesitter",
    -- We recommend updating the parsers on update
    run = ":TSUpdate",
  },
  -- Allows for LSP refactoring.
  { "nvim-treesitter/nvim-treesitter-refactor" },
  -- Use treesitter objects as Vim text objects for selection
  { "nvim-treesitter/nvim-treesitter-textobjects" },

  { "voldikss/vim-floaterm" },

  -- File tree explorer
  {
    "kyazdani42/nvim-tree.lua",
    dependencies = {
      "kyazdani42/nvim-web-devicons", -- optional, for file icon
    },
  },

  {
    "francoiscabrol/ranger.vim",
    dependencies = {
      "rbgrouleff/bclose.vim",
    },
  },

  { "b3nj5m1n/kommentary" },

  -- Org mode clone for Neovim
  { "kristijanhusak/orgmode.nvim" },
  {
    "akinsho/org-bullets.nvim",
    config = function()
      require("org-bullets").setup({
        symbols = { "◉", "○", "✸", "✿" },
      })
    end,
  },

  -- SQL
  -- PostgreSQL syntax highlighting
  { "lifepillar/pgsql.vim" },

  -- Formatter
  { "stevearc/conform.nvim" },
}
