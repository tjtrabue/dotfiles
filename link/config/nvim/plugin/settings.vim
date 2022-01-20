" All Neovim settings.

" General {{{
" Make Vim more useful
set nocompatible

" Use the OS clipboard by default (on versions compiled with `+clipboard`)
" NOTE: tmux interferes with the unnamed register, so it may be beneficial
" to disable this feature when using tmux.
"if !exists('$TMUX')
set clipboard=unnamedplus
"endif

" Allow backspace in insert mode
set backspace=indent,eol,start

" Optimize for fast terminal connections
set ttyfast

" Add the g flag to search/replace by default
set gdefault

" Use UTF-8 without BOM
set encoding=utf-8 nobomb

" Don’t add empty newlines at the end of files
set binary
set noeol

" Use F2 to toggle paste mode for pasting in text from the system clipboard
set pastetoggle=<F2>

" Set to auto read when a file is changed from the outside
set autoread

" Tell Vim where to find files when autocompleting. Vim first looks in the directory containing the
" current file (.), then the current working directory (,,), then each directory under the
" current directory (**)
set path=.,,**

" Turn on spell checking for many document types
autocmd BufRead,BufNewFile *.tex,*.html,*.txt,*.md
      \ set spelllang=en_us spell
" }}}

" Autocompletion {{{
set completeopt=menuone,noselect
" }}}

" Backups {{{
" Create backup dirs if they do not already exist
silent !sh -c 'mkdir -p ${XDG_CONFIG_HOME:-${HOME}/.config}/nvim/backups >>/dev/null 2>&1'
silent !sh -c 'mkdir -p ${XDG_CONFIG_HOME:-${HOME}/.config}/nvim/swaps >>/dev/null 2>&1'
silent !sh -c 'mkdir -p ${XDG_CONFIG_HOME:-${HOME}/.config}/nvim/undo >>/dev/null 2>&1'

" Make variables for backup, swap, and undo files.
let s:user_config_dir = $XDG_CONFIG_HOME
if ! isdirectory(s:user_config_dir)
  let s:user_config_dir = $HOME . '/.config'
endif
let s:user_nvim_backups_dir = s:user_config_dir . '/nvim/backups'
let s:user_nvim_swaps_dir = s:user_config_dir . '/nvim/swaps'
let s:user_nvim_undo_dir = s:user_config_dir . '/nvim/undo'

" Centralize backups, swapfiles and undo history
set backup
set writebackup
" Notice the use of 'let &...' instead of 'set ...'. Using 'let' allows us to
" use variables on the right side of option assignments.
let &backupdir=s:user_nvim_backups_dir
let &directory=s:user_nvim_swaps_dir
if exists('&undodir')
  let &undodir=s:user_nvim_undo_dir
endif

" Don’t create backups when editing files in certain directories
set backupskip=/tmp/*,/private/tmp/*

" Set lines of history for Vim to remember to 700
set history=700

" Open files with cursor at last edited position
au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$")
      \| exe "normal! g'\"" | endif
" }}}

" Buffers {{{
" Hide old buffers instead of close them when editing a new one. That is, allow
" the user to have unsaved changes in a buffer and still be able to open a new
" one.
set hidden

" Remember info about open buffers on close
set viminfo^=%
" }}}

" Code Folding {{{
" Code folding is Vim-speak for code block collapsing. It allows for a birdseye
" view of large buffers.

" Basic command reference:
"   Open all folds under fold at point  : zO
"   Close all folds under fold at point : zC
"   Open all folds in buffer            : zR
"   Close all folds in buffer           : zM

" Enable folding.
set foldenable

" The number of fold occurrences after which Vim will start folding code.
set foldlevel=0

" The maximum number of folds to nest within one another.
set foldnestmax=10

" Default fold method to use.
" NOTE: See also the 'autocmds.vim' file for more fold settings.
set foldmethod=indent
" }}}

" Indentation {{{
" Use auto-indent
set autoindent
filetype indent plugin on
set smartindent
" }}}
"
" Searching {{{
" Use normal regular expression syntax and character meaning
" (should be on by default anyway)
set magic

" Highlight searches
set hlsearch

" Highlight dynamically as pattern is typed
set incsearch

" Ignore case when searching
set ignorecase

" Turn on case-sensitivity if user types an upper-case letter
set smartcase

" Make searches wrap around the end of file
set wrapscan
" }}}

" Tabs and Spaces {{{
" Translate tabs to spaces
set expandtab

" Set tab size to 4 spaces
set tabstop=2

" Don't use soft tabs, so as to avoid compatibility errors with other editors
set softtabstop=2
set shiftwidth=2

" Use multiples of shiftwidth when indenting with '<' and '>'
set shiftround

" Insert tabs on the start of a line according to shiftwidth, not tabstop
set smarttab
" }}}

" UI Config {{{
" Draw a vertical ruler at 1 character after textwidth to designate max width
" for coding
set colorcolumn=+1

" Set text width to 80 characters
set textwidth=80

" Wrap lines
set wrap

" Copy the previous indentation on autoindenting
set copyindent

" Enhance command-line completion
set wildmenu

" Ignore these file extensions when autocompleting
set wildignore=*.swp,*.bak,*.pyc,*.class

" Only redraw screen when needed (makes for faster macros)
set lazyredraw

" Show matching [{()}]
set showmatch

" Show partial commands in the last line of the screen
set showcmd

" Enable syntax highlighting
syntax enable

" Respect modeline in files
set modeline
set modelines=1

" Enable per-directory .vimrc files and disable unsafe commands in them
set exrc
set secure

" Highlight current line
set cursorline

" Show “invisible” characters
" Mark lines that extend past the end of the screen with a '#'
set lcs=tab:▸\ ,trail:·,extends:#,eol:¬,nbsp:_
set list

" Always show status line
set laststatus=2

" Enable mouse in all modes
set mouse=a

" Disable error bell
set noerrorbells

" Use visual bell instead of beeping when doing something wrong
set visualbell

" And reset the terminal code for the visual bell. If visualbell is set, and
" this line is also included, vim will neither flash nor beep. If visualbell
" is unset, this does nothing.
set t_vb=

" Don’t reset cursor to start of line when moving around
set nostartofline

" Show the cursor position
set ruler

" Don’t show the intro message when starting Vim
set shortmess=atI

" Show the current mode
set showmode

" Show the filename in the window titlebar
set title

" Show the (partial) command as it’s being typed
set showcmd

" Set the command window height to 2 lines, to avoid many cases of having to
" 'press <Enter> to continue'
set cmdheight=2

" Use hybrid line numbers (found by setting both number and relativenumber)
if exists("&relativenumber")
  set number relativenumber
  au BufReadPost * set number relativenumber
endif

" Switch to absolute line numbers if Vim loses focus
:au FocusLost * :set number norelativenumber
:au FocusGained * :set number relativenumber

" Start scrolling three lines before the horizontal window border
set scrolloff=3

" Start scrolling three columns before vertical border of window
set sidescrolloff=3

" Cause pane splits to occur below (for horizontal split) and to the right
" (for vertical splits)
set splitbelow
set splitright
" }}}

" vim:foldenable:foldmethod=marker
