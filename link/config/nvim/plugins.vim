" Neovim Plugins
" Plugins MUST be specified in this file in order to load properly!

" Install vim-plug if not already done
if !filereadable(stdpath('data') . '/site/autoload/plug.vim')
  silent !sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" Specify a directory for plugins and begin installation
call plug#begin(stdpath('data') . '/plugged')

" Specify plugins {{{
" APIs
Plug 'jparise/vim-graphql'

" Status
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'edkolev/tmuxline.vim'

" Colors and Themes
Plug 'rafi/awesome-vim-colorschemes'

" Icons
Plug 'ryanoasis/vim-devicons'

" Searching
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all'}
Plug 'junegunn/fzf.vim'
Plug 'shougo/neomru.vim'

" Version Control
Plug 'airblade/vim-gitgutter'

" Utilities
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'easymotion/vim-easymotion'
Plug 'haya14busa/incsearch.vim'
Plug 'haya14busa/incsearch-easymotion.vim'
Plug 'junegunn/vim-easy-align'
Plug 'tpope/vim-commentary'
Plug 'sjl/gundo.vim'
Plug 'terryma/vim-multiple-cursors'
Plug 'tpope/vim-bundler'
Plug 'tpope/vim-surround'
Plug 'godlygeek/tabular'
Plug 'Shougo/unite.vim'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-eunuch'

" Neovim completion
Plug 'hrsh7th/nvim-compe'

" Debugging
" Neovim frontend to GDB
Plug 'sakhnik/nvim-gdb', { 'do': ':!./install.sh \| UpdateRemotePlugins' }
" Markup
Plug 'mattn/emmet-vim'
Plug 'plasticboy/vim-markdown'
Plug 'shime/vim-livedown'

" TOML
Plug 'cespare/vim-toml'

" C/C++
Plug 'vim-scripts/c.vim', {'for': ['c', 'c++']}

" CSS/LESS/SCSS
Plug 'hail2u/vim-css3-syntax'
Plug 'ap/vim-css-color'
Plug 'groenewege/vim-less'
Plug 'cakebaker/scss-syntax.vim'

" Dart
Plug 'dart-lang/dart-vim-plugin'

" Javascript
Plug 'pangloss/vim-javascript', { 'for': ['javascript',  'javascript.jsx' ]}
Plug 'othree/javascript-libraries-syntax.vim', { 'for': ['javascript', 'javascript.jsx']}
Plug 'burnettk/vim-angular', { 'for': ['javascript', 'javascript.jsx']}
Plug 'mxw/vim-jsx', { 'for': ['javascript', 'javascript.jsx']}
Plug 'ternjs/tern_for_vim', { 'for': ['javascript', 'javascript.jsx']}
"" NodeJS
Plug 'moll/vim-node'

" Ruby
Plug 'tpope/vim-rails', {'for': 'ruby'}

" Perl
Plug 'vim-perl/vim-perl', { 'for': 'perl', 'do': 'make clean carp dancer highlight-all-pragmas moose test-more try-tiny' }

" Haskell
Plug 'neovimhaskell/haskell-vim', {'for': 'haskell'}

" Elixir
Plug 'elixir-editors/vim-elixir', {'for': 'elixir'}
Plug 'mhinz/vim-mix-format', {'for': 'elixir'}

" LaTeX
Plug 'vim-latex/vim-latex', {'for': 'tex'}
Plug 'lervag/vimtex', {'for': 'tex'}
Plug 'xuhdev/vim-latex-live-preview', {'for': 'tex'}

" Editor
Plug 'editorconfig/editorconfig-vim'
Plug 'junegunn/goyo.vim'
Plug 'vim-syntastic/syntastic'
Plug 'tpope/vim-repeat'
Plug 'jiangmiao/auto-pairs'
Plug 'tpope/vim-endwise'
" Plug 'nathanaelkane/vim-indent-guides'
Plug 'glepnir/indent-guides.nvim'
Plug 'mhinz/vim-startify'

" Adds more text objects to Vim on which to operate.
Plug 'wellle/targets.vim'
"
Plug 'majutsushi/tagbar'

" LSP: Language Server Protocol
Plug 'neovim/nvim-lspconfig'

" Telescope
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'
Plug 'nvim-telescope/telescope-fzy-native.nvim'

" neovim-tree explorer.
Plug 'kyazdani42/nvim-web-devicons' " for file icons
Plug 'kyazdani42/nvim-tree.lua'

" treesitter.
" After installing treesitter, run `:TSInstall all`
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}  " We recommend updating the parsers on update

" which-key
Plug 'liuchengxu/vim-which-key'

" Lazygit is a great interface for Git.
Plug 'kdheepak/lazygit.nvim'

" A floating terminal window for Neovim.
Plug 'voldikss/vim-floaterm'
" }}}

" Initialize plugin system
call plug#end()

" Automatically install missing plugins on startup {{{
autocmd VimEnter *
  \  if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
  \|   PlugInstall --sync | q
  \| endif
" }}}

" Use `gx` to open the GitHub URL for a plugin or commit with default browser {{{
function! s:plug_gx()
  let line = getline('.')
  let sha  = matchstr(line, '^  \X*\zs\x\{7,9}\ze ')
  let name = empty(sha) ? matchstr(line, '^[-x+] \zs[^:]\+\ze:')
                      \ : getline(search('^- .*:$', 'bn'))[2:-2]
  let uri  = get(get(g:plugs, name, {}), 'uri', '')
  if uri !~ 'github.com'
    return
  endif
  let repo = matchstr(uri, '[^:/]*/'.name)
  let url  = empty(sha) ? 'https://github.com/'.repo
                      \ : printf('https://github.com/%s/commit/%s', repo, sha)
  call netrw#BrowseX(url, 0)
endfunction

augroup PlugGx
  autocmd!
  autocmd FileType vim-plug nnoremap <buffer> <silent> gx :call <sid>plug_gx()<cr>
augroup END
" }}}

" vim:foldenable:foldmethod=marker:foldlevel=0
