-- This file can be loaded by calling `lua require('plugins')` from your init.vim

-- Bootstrapping packer {{{
local execute = vim.api.nvim_command
local fn = vim.fn

-- Where to install packer.
local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"

if fn.empty(fn.glob(install_path)) > 0 then
  execute("!git clone https://github.com/wbthomason/packer.nvim " .. install_path)
  execute "packadd packer.nvim"
end
-- }}}

-- Only required if you have packer in your `opt` pack
-- vim.cmd [[packadd packer.nvim]]

-- Only if your version of Neovim doesn't have https://github.com/neovim/neovim/pull/12632 merged
-- vim._update_package_paths()

-- Explanation of plugin options {{{
--[[ use {
  'myusername/example',        -- The plugin location string
  -- The following keys are all optional
  disable = boolean,           -- Mark a plugin as inactive
  as = string,                 -- Specifies an alias under which to install the plugin
  installer = function,        -- Specifies custom installer. See "custom installers" below.
  updater = function,          -- Specifies custom updater. See "custom installers" below.
  after = string or list,      -- Specifies plugins to load before this plugin. See "sequencing" below
  rtp = string,                -- Specifies a subdirectory of the plugin to add to runtimepath.
  opt = boolean,               -- Manually marks a plugin as optional.
  branch = string,             -- Specifies a git branch to use
  tag = string,                -- Specifies a git tag to use
  commit = string,             -- Specifies a git commit to use
  lock = boolean,              -- Skip this plugin in updates/syncs
  run = string or function,    -- Post-update/install hook. See "update/install hooks".
  requires = string or list,   -- Specifies plugin dependencies. See "dependencies".
  rocks = string or list,      -- Specifies Luarocks dependencies for the plugin
  config = string or function, -- Specifies code to run after this plugin is loaded.
  -- The setup key implies opt = true
  setup = string or function,  -- Specifies code to run before this plugin is loaded.
  -- The following keys all imply lazy-loading and imply opt = true
  cmd = string or list,        -- Specifies commands which load this plugin.
  ft = string or list,         -- Specifies filetypes which load this plugin.
  keys = string or list,       -- Specifies maps which load this plugin. See "Keybindings".
  event = string or list,      -- Specifies autocommand events which load this plugin.
  fn = string or list          -- Specifies functions which load this plugin.
  cond = string, function, or list of strings/functions,   -- Specifies a conditional test to load this plugin
  module = string or list      -- Specifies patterns (e.g. for string.match) of Lua module names which, when required, load this plugin
} ]]
-- }}}

local packer = require("packer")

-- Set some default configuration/behavior for Packer {{{
packer.init(
  {
    -- For some reason the author didn't provide a default thread limit for
    -- asynchronous jobs. If you omit this limit, Packer can fail to
    -- install/update plugins on some high-performance computers.
    max_jobs = 8
  }
)
-- }}}

-- Specify plugins {{{
packer.startup(
  function()
    -- Packer can manage itself
    use {"wbthomason/packer.nvim"}

    use {"jparise/vim-graphql"}

    use {"edkolev/tmuxline.vim"}

    use {"glepnir/galaxyline.nvim"}

    -- Tab bar {{{
    use {"romgrk/barbar.nvim"}
    -- }}}

    -- Colorschemes {{{
    use {"rafi/awesome-vim-colorschemes"}
    -- }}}

    -- Fonts/icons {{{
    use {"kyazdani42/nvim-web-devicons"}
    -- }}}

    use {"shougo/neomru.vim"}

    -- Git (((
    use {"airblade/vim-gitgutter"}
    use {"kdheepak/lazygit.nvim"}
    use {"TimUntersberger/neogit"}
    use {"f-person/git-blame.nvim"}
    -- )))

    -- Snippets {{{
    use {"SirVer/ultisnips"}
    use {"honza/vim-snippets"}
    -- }}}

    use {"haya14busa/incsearch.vim"}

    use {"haya14busa/incsearch-easymotion.vim"}

    -- Multiple cursor support
    use {"mg979/vim-visual-multi"}

    use {"tpope/vim-bundler"}

    use {"tpope/vim-surround"}

    use {"godlygeek/tabular"}

    use {"Shougo/unite.vim"}

    use {"tpope/vim-unimpaired"}

    use {"tpope/vim-vinegar"}

    use {"tpope/vim-eunuch"}

    use {
      "mbbill/undotree",
      config = [[vim.g.undotree_SetFocusWhenToggle = 1]]
    }

    use {"hrsh7th/nvim-compe"}

    use {"sakhnik/nvim-gdb"}

    -- Markdown {{{
    use {"plasticboy/vim-markdown"}
    use {"shime/vim-livedown"}
    use {"npxbr/glow.nvim", run = ":GlowInstall"}
    -- }}}

    -- TOML {{{
    use {"cespare/vim-toml"}
    -- }}}

    use {"vim-scripts/c.vim"}

    -- WWWW technologies {{{
    use {"hail2u/vim-css3-syntax"}
    use {"ap/vim-css-color"}
    use {"groenewege/vim-less"}
    use {"dart-lang/dart-vim-plugin"}
    use {"mxw/vim-jsx"}
    use {"mattn/emmet-vim"}
    -- }}}

    -- Ruby {{{
    use {"tpope/vim-rails"}
    -- }}}

    -- Perl {{{
    use {"vim-perl/vim-perl"}
    -- }}}

    -- Haskell {{{
    use {"neovimhaskell/haskell-vim"}
    -- }}}

    -- LaTeX {{{
    use {"lervag/vimtex"}
    use {"xuhdev/vim-latex-live-preview"}
    -- }}}

    -- Editing {{{
    use {"tpope/vim-repeat"}
    use {"wellle/targets.vim"}
    use {"easymotion/vim-easymotion"}
    use {"editorconfig/editorconfig-vim"}
    -- }}}

    -- Delimiter plugins {{{
    use {"jiangmiao/auto-pairs"}
    use {"tpope/vim-endwise"}
    -- }}}

    -- UI {{{
    use {"glepnir/indent-guides.nvim"}
    use {"nvim-lua/popup.nvim"}
    use {"kevinhwang91/nvim-hlslens"}
    use {"liuchengxu/vim-which-key"}
    -- }}}

    use {"mhinz/vim-startify"}

    use {"majutsushi/tagbar"}

    -- Language Server Protocol
    use {"neovim/nvim-lspconfig"}

    -- Debugging
    use {"mfussenegger/nvim-dap"}
    use {"puremourning/vimspector"}

    -- Movement {{{
    use {"chaoren/vim-wordmotion"}
    use {"justinmk/vim-sneak"}
    -- }}}

    -- Prettification {{{
    use {"junegunn/vim-easy-align"}
    use {"mhartington/formatter.nvim"}
    -- }}}

    use {"nvim-lua/plenary.nvim"}

    -- Fuzzy searching {{{
    use {"nvim-telescope/telescope.nvim"}
    use {"nvim-telescope/telescope-fzy-native.nvim"}
    -- }}}

    -- Syntax parsing {{{
    use {"sheerun/vim-polyglot"}
    use {
      "nvim-treesitter/nvim-treesitter",
      -- We recommend updating the parsers on update}
      run = ":TSUpdate"
    }
    -- }}}

    -- Terminal {{{
    use {"voldikss/vim-floaterm"}
    -- }}}

    -- File tree explorer
    use {
      "kyazdani42/nvim-tree.lua",
      cmd = {"NvimTreeOpen", "NvimTreeToggle"}
    }

    use {
      "francoiscabrol/ranger.vim",
      requires = {{"rbgrouleff/bclose.vim"}}
    }

    -- Commenting code {{{
    use {"b3nj5m1n/kommentary"}
    -- }}}
  end
)
-- }}}

-- Install any uninstalled plugins.
packer.install()

return packer

-- vim:foldenable:foldmethod=marker:foldlevel=0
