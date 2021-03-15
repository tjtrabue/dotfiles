-- This file can be loaded by calling `lua require('plugins')` from your init.vim

-- Bootstrapping packer {{{
local execute = vim.api.nvim_command
local fn = vim.fn

-- Where to install packer.
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
    execute('!git clone https://github.com/wbthomason/packer.nvim '..install_path)
    execute 'packadd packer.nvim'
end
-- }}}

-- Only required if you have packer in your `opt` pack
-- vim.cmd [[packadd packer.nvim]]

-- Only if your version of Neovim doesn't have https://github.com/neovim/neovim/pull/12632 merged
-- vim._update_package_paths()

-- use package statements {{{
return require('packer').startup(function()
    -- Packer can manage itself as an optional plugin
    use {'wbthomason/packer.nvim'}

    use {'jparise/vim-graphql'}
    use {'edkolev/tmuxline.vim'}
    use {'glepnir/galaxyline.nvim'}
    use {'romgrk/barbar.nvim'}
    use {'rafi/awesome-vim-colorschemes'}
    use {'kyazdani42/nvim-web-devicons'}
    use {'junegunn/fzf'}
    use {'junegunn/fzf.vim'}
    use {'shougo/neomru.vim'}
    use {'airblade/vim-gitgutter'}
    use {'SirVer/ultisnips'}
    use {'honza/vim-snippets'}
    use {'easymotion/vim-easymotion'}
    use {'haya14busa/incsearch.vim'}
    use {'haya14busa/incsearch-easymotion.vim'}
    use {'junegunn/vim-easy-align'}
    use {'terryma/vim-multiple-cursors'}
    use {'tpope/vim-bundler'}
    use {'tpope/vim-surround'}
    use {'godlygeek/tabular'}
    use {'Shougo/unite.vim'}
    use {'tpope/vim-unimpaired'}
    use {'tpope/vim-vinegar'}
    use {'tpope/vim-eunuch'}
    use {'mbbill/undotree'}
    use {'hrsh7th/nvim-compe'}
    use {'sheerun/vim-polyglot'}
    use {'sakhnik/nvim-gdb'}
    use {'mattn/emmet-vim'}
    use {'plasticboy/vim-markdown'}
    use {'shime/vim-livedown'}
    use {'npxbr/glow.nvim'}
    use {'cespare/vim-toml'}
    use {'vim-scripts/c.vim'}
    use {'hail2u/vim-css3-syntax'}
    use {'ap/vim-css-color'}
    use {'groenewege/vim-less'}
    use {'dart-lang/dart-vim-plugin'}
    use {'pangloss/vim-javascript'}
    use {'othree/javascript-libraries-syntax.vim'}
    use {'burnettk/vim-angular'}
    use {'mxw/vim-jsx'}
    use {'ternjs/tern_for_vim'}
    use {'moll/vim-node'}
    use {'tpope/vim-rails'}
    use {'vim-perl/vim-perl'}
    use {'neovimhaskell/haskell-vim'}
    use {'vim-latex/vim-latex'}
    use {'lervag/vimtex'}
    use {'xuhdev/vim-latex-live-preview'}
    use {'editorconfig/editorconfig-vim'}
    use {'junegunn/goyo.vim'}
    use {'tpope/vim-repeat'}
    use {'jiangmiao/auto-pairs'}
    use {'tpope/vim-endwise'}
    use {'glepnir/indent-guides.nvim'}
    use {'mhinz/vim-startify'}
    use {'wellle/targets.vim'}
    use {'majutsushi/tagbar'}
    use {'neovim/nvim-lspconfig'}
    use {'nvim-lua/popup.nvim'}
    use {'nvim-lua/plenary.nvim'}
    use {'nvim-telescope/telescope.nvim'}
    use {'nvim-telescope/telescope-fzy-native.nvim'}
    use {
        'nvim-treesitter/nvim-treesitter',
        -- We recommend updating the parsers on update}
        run = ':TSUpdate'
    }
    use {'liuchengxu/vim-which-key'}
    use {'kdheepak/lazygit.nvim'}
    use {'voldikss/vim-floaterm'}
    use {'kyazdani42/nvim-tree.lua'}
    use {'b3nj5m1n/kommentary'}
    use {'TimUntersberger/neogit'}
    use {'f-person/git-blame.nvim'}
    use {'kevinhwang91/nvim-hlslens'}
end)
-- }}}

-- vim:foldenable:foldmethod=marker:foldlevel=0
