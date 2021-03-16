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

-- Optional settings for plugins {{{
--   cmd    -> Only load plugin after command(s) are run; used for lazy-loading.
--   config -> Run lua code after plugin loads.
--   opt    -> Mark plugin as optional, instead of required.
--   run    -> Run lua function, Vim commands, or Shell commands after plugin
--             installs/updates.
-- }}}

local packer = require("packer")

-- Set some default configuration/behavior for Packer {{{
packer.init(
  {
    -- For some reason the author didn't provide a default thread limit for
    -- asynchronous jobs. If you omit this limit, Packer can fail to
    -- install/update plugins on some high-performance computers.
    max_jobs = 16
  }
)
-- }}}
--
-- Specify plugins {{{
packer.startup(
  function()
    -- Packer can manage itself as an optional plugin
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
      opt = true,
      cmd = {"UndotreeToggle"},
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
    use {"justinmk/vim-sneak", config = [[require('config.sneak')]]}
    -- }}}

    -- Prettification {{{
    use {"junegunn/vim-easy-align", config = [[require('config.easy_align')]]}
    use {"mhartington/formatter.nvim", config = [[require('config.format')]]}
    -- }}}

    use {"nvim-lua/plenary.nvim"}

    -- Fuzzy searching {{{
    use {
      "junegunn/fzf",
      run = "cd ~/.fzf && ./install --all"
    }
    use {"junegunn/fzf.vim"}
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
      opt = true,
      cmd = {"NvimTreeOpen", "NvimTreeToggle"}
    }

    -- Commenting code {{{
    use {"b3nj5m1n/kommentary"}
    -- }}}
  end
)
-- }}}

-- Install new plugins on startup.
local packer_plugins_dir = os.getenv("HOME") .. "/.local/share/nvim/site/pack/packer"
local start_dir = packer_plugins_dir .. "/start"
local opt_dir = packer_plugins_dir .. "/opt"

--[[ for key, value in pairs(packer.plugins) do
  print("Plugin key:" .. key)
  print("Plugin value:" .. value)
end ]]
-- packer.install()

return packer

-- vim:foldenable:foldmethod=marker:foldlevel=0
