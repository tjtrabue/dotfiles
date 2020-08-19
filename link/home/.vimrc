" Author: Thomas J. Trabue
" Date: 11/26/2019

" Neovim config {{{
if has('nvim')
  " Currently empty.
  " Neovim 4.0 seems happier to figure things out for itself.
endif
" }}}"

" Plugins {{{
" vim-plug
" Install vim-plug if not already done
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" Specify a directory for plugins and begin installation
let s:vim_plug_base_dir = 'plugged'
let s:vim_plug_dir = '~/.vim/' . s:vim_plug_base_dir
if has('nvim')
  call plug#begin(stdpath('data') . s:vim_plug_base_dir)
endif
call plug#begin(s:vim_plug_dir)

" Specify plugins
" APIs {{{
Plug 'jparise/vim-graphql'
" }}}
" Linting {{{
" NOTE: Currently disabled in favor of using coc.nvim
" Plug 'w0rp/ale'
" }}}
" Status {{{
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'edkolev/tmuxline.vim'
" }}}
" Colors and Themes {{{
Plug 'drewtempelmeyer/palenight.vim'
Plug 'hzchirs/vim-material'
Plug 'kyoz/purify', { 'rtp': 'vim' }
Plug 'chriskempson/tomorrow-theme', { 'rtp': 'vim' }
" Icons
Plug 'ryanoasis/vim-devicons'
" }}}
" Searching {{{
" NOTE: fzf not needed anymore thanks to coc.nvim's file list feature
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all'}
Plug 'junegunn/fzf.vim'
Plug 'shougo/neomru.vim'
" }}}
" Version Control {{{
Plug 'lambdalisue/gina.vim'
Plug 'airblade/vim-gitgutter'
" }}}
" Utilities {{{
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'easymotion/vim-easymotion'
Plug 'haya14busa/incsearch.vim'
Plug 'haya14busa/incsearch-easymotion.vim'
Plug 'junegunn/vim-easy-align'
Plug 'tpope/vim-commentary'

" Fancy new file explorer plugin
if has('nvim')
  Plug 'Shougo/defx.nvim', { 'do': ':UpdateRemotePlugins' }
else
  Plug 'Shougo/defx.nvim'
  Plug 'roxma/nvim-yarp'
  Plug 'roxma/vim-hug-neovim-rpc'
endif

Plug 'sjl/gundo.vim'
Plug 'terryma/vim-multiple-cursors'
Plug 'tpope/vim-bundler'
Plug 'tpope/vim-surround'
Plug 'godlygeek/tabular'
Plug 'Shougo/unite.vim'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-vinegar'
" }}}
" Debugging {{{
if has('nvim')
  " Neovim frontend to GDB
  Plug 'sakhnik/nvim-gdb', { 'do': ':!./install.sh \| UpdateRemotePlugins' }
endif
" }}}
" Markup {{{
Plug 'mattn/emmet-vim'
Plug 'plasticboy/vim-markdown'
Plug 'shime/vim-livedown'
" }}}
" Autocompletion / IDE Integration {{{
" deoplete and deoplete plugins (DISABLED in favor of coc.nvim) {{{
" if has('nvim')
"   Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
" else
"   Plug 'Shougo/deoplete.nvim'
"   " Plug 'roxma/nvim-yarp'
"   Plug 'roxma/vim-hug-neovim-rpc'
" endif
" Plug 'autozimu/LanguageClient-neovim', {
"       \ 'branch': 'next',
"       \ 'do': 'bash install.sh'
"       \}
" Plug 'carlitux/deoplete-ternjs'
" Plug 'zchee/deoplete-clang'
" Plug 'zchee/deoplete-jedi'
" Plug 'poppyschmo/deoplete-latex'
" Plug 'zchee/deoplete-zsh'
" }}}
Plug 'Shougo/neco-vim'
Plug 'eagletmt/neco-ghc'
Plug 'slashmili/alchemist.vim'
Plug 'mhartington/nvim-typescript'
" coc.nvim {{{
" Installed from latest tag
Plug 'neoclide/coc.nvim', {'tag': '*', 'branch': 'release'}
Plug 'neoclide/coc-neco'
Plug 'jackguo380/vim-lsp-cxx-highlight'
" }}}
" }}}
" C/C++ {{{
Plug 'vim-scripts/c.vim', {'for': ['c', 'c++']}
" }}}
" CSS/LESS/SCSS {{{
Plug 'hail2u/vim-css3-syntax'
Plug 'ap/vim-css-color'
Plug 'groenewege/vim-less'
Plug 'cakebaker/scss-syntax.vim'
" }}}
" Go {{{
" WARNING: This plugin is real heavy! It updates very slowly. It also may not be
"          necessary now that coc.nvim exists.
" Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
" }}}
" Javascript {{{
Plug 'pangloss/vim-javascript', { 'for': ['javascript',  'javascript.jsx' ]}
Plug 'othree/javascript-libraries-syntax.vim', { 'for': ['javascript', 'javascript.jsx']}
Plug 'burnettk/vim-angular', { 'for': ['javascript', 'javascript.jsx']}
Plug 'mxw/vim-jsx', { 'for': ['javascript', 'javascript.jsx']}
Plug 'ternjs/tern_for_vim', { 'for': ['javascript', 'javascript.jsx']}
" NodeJS
Plug 'moll/vim-node'
" }}}
" Ruby {{{
Plug 'tpope/vim-rails', {'for': 'ruby'}
" }}}
" Perl {{{
Plug 'vim-perl/vim-perl', { 'for': 'perl', 'do': 'make clean carp dancer highlight-all-pragmas moose test-more try-tiny' }
" }}}
" Haskell {{{
Plug 'neovimhaskell/haskell-vim', {'for': 'haskell'}
" }}}
" Elixir {{{
Plug 'elixir-editors/vim-elixir', {'for': 'elixir'}
Plug 'mhinz/vim-mix-format', {'for': 'elixir'}
" }}}
" LaTeX {{{
Plug 'vim-latex/vim-latex', {'for': 'tex'}
Plug 'lervag/vimtex', {'for': 'tex'}
Plug 'xuhdev/vim-latex-live-preview', {'for': 'tex'}
" }}}
" Editor {{{
Plug 'editorconfig/editorconfig-vim'
Plug 'junegunn/goyo.vim'
Plug 'vim-syntastic/syntastic'
Plug 'tpope/vim-repeat'
Plug 'jiangmiao/auto-pairs'
Plug 'tpope/vim-endwise'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'mhinz/vim-startify'
" }}}
" Filesystem {{{
Plug 'tpope/vim-eunuch'
" }}}
" Tags {{{
Plug 'majutsushi/tagbar'
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
" }}}

" General {{{
" Make Vim more useful
set nocompatible

" Use the OS clipboard by default (on versions compiled with `+clipboard`)
" NOTE: tmux interferes with the unnamed register, so it may be beneficial
" to disable this feature when using tmux.
"if !exists('$TMUX')
    set clipboard=unnamed
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

" Backups {{{
" Centralize backups, swapfiles and undo history
set backup
set writebackup
set backupdir=~/.vim/backups
set directory=~/.vim/swaps
if exists('&undodir')
    set undodir=~/.vim/undo
endif

" Don’t create backups when editing files in certain directories
set backupskip=/tmp/*,/private/tmp/*

" Set lines of history for Vim to remember to 700
set history=700

" Open files with cursor at last edited position
if has('autocmd')
    au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$")
        \| exe "normal! g'\"" | endif
endif
" }}}

" UI Config {{{
" Draw a vertical ruler at 1 character after textwidth to designate max width
" for coding
set colorcolumn=+1
:highlight ColorColumn ctermbg=lightgrey guibg=lightgrey

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

" Indentation {{{
" Use auto-indent
set autoindent
filetype indent plugin on
set smartindent

" Emulate TextMate's shift left/right key commands
nmap <D-[> <<
nmap <D-]> >>
vmap <D-[> <gv
vmap <D-]> >gv
" }}}

" Folding {{{
" Basic command reference:
"   Open all folds  : zR
"   Close all folds : zM

" Enable folding (code block collapsing)
set foldenable

" Start folding from the very beginning
set foldlevel=0

" Nest at most 10 folds
set foldnestmax=10

" Fold based on language syntax by default
set foldmethod=syntax
" }}}

" Movement {{{
" Disable arrow keys in all modes *gasp!*
" I know, but get over it. You need to learn to use Vim's intended movement
" buttons, and trust me, you'll thank me when you do.
map <UP> <NOP>
map <DOWN> <NOP>
map <LEFT> <NOP>
map <RIGHT> <NOP>
imap <UP> <NOP>
imap <DOWN> <NOP>
imap <LEFT> <NOP>
imap <RIGHT> <NOP>

" Move up and down by visual line, not actual line.
" i.e., if a line gets visually wrapped due to your text wrapping setting, j will not skip
" over the "fake" part of the visual line in favor of the "real" one.
nnoremap j gj
nnoremap k gk

" Highlight last inserted text
nnoremap gV `[v`]
" }}}

" Normal Mode Mappings {{{
" Use 'QQ' to exit without saving
noremap QQ :qa!<CR>

" Use 'ZC' to enter command mode
noremap ZC :

" Move lines with Alt-j and Alt-k
nnoremap <A-j> :m .+1<CR>==
nnoremap <A-k> :m .-2<CR>==

" Make 'Y' yank to the end of the current line.
" This makes 'Y' behave more like other 'capital letter' commands.
nnoremap Y y$
" }}}

" Insert Mode Mappings {{{
" Use 'jk' as an Escape alias to exit insert and visual modea.
inoremap jk <ESC>

" Move lines with Alt-j and Alt-k
inoremap <A-j> <Esc>:m .+1<CR>==gi
inoremap <A-k> <Esc>:m .-2<CR>==gi
" }}}

" Terminal Mode Mappings {{{
" Use Esc to exit terminal mode
tnoremap <Esc> <C-\><C-n>
" Also use Alt+[ to exit terminal mode
tnoremap <A-[> <Esc>
" }}}

" Visual Mode Mappings {{{
" Move lines with Alt-j and Alt-k
vnoremap <A-j> :m '>+1<CR>gv=gv
vnoremap <A-k> :m '<-2<CR>gv=gv
" }}}

" Leader Shortcuts {{{
" Change mapleader
let mapleader=","

" Save session for restoring later with 'vim -S' (,ss)
map <leader>ss :mksession<CR>

" Save a file as root (,sr)
map <leader>sr :w !sudo tee % > /dev/null<CR>

" Use ',t' to toggle absolute and relative line numbers
map <leader>t :call ToggleNumber()<CR>

" Open ag.vim with '\'
map \ :Ag<SPACE>

" Use ',.' to go to end of current line
map <leader>. $

" Use ',m' to go to beginning of current line
map <leader>m ^

" Use ',f' in normal mode to open / close folds
map <leader>f za

" Pasting from clipboard into vim is formatted correctly (,p)
map <silent> <leader>p :set paste<CR>"*p:set nopaste<CR>

" Quickly edit/reload the ~/.vimrc file
" ',ev' opens ~/.vimrc in a new buffer to edit.
" ',sv' sources the ~/.vimrc file.
map <silent> <leader>ev :e $MYVIMRC<CR>
map <silent> <leader>sv :so $MYVIMRC<CR>:call SourceConfigs()<CR>

" Switch CWD to the directory of the open buffer
map <leader>cd :cd %:p:h<cr>:pwd<cr>
" }}}

" Tab Configuration {{{
" Basic commands:
"   Swtich to next tab     : gt
"   Switch to previous tab : gT

" Useful mappings for managing tabs
map <C-i>n :tabnew<CR>
map <C-i>o :tabonly<CR>
map <C-i>c :tabclose<CR>
map <C-i>m :tabmove<CR>

" Tab navigation mappings
map <C-i>h :tabprevious<CR>
map <C-i>j :tabrewind<CR>
map <C-i>k :tablast<CR>
map <C-i>l :tabnext<CR>

" Opens a new tab with the current buffer's path
" Super useful when editing files in the same directory
map <C-i>e :tabedit <C-r>=expand("%:p:h")<CR>/
" }}}

" Buffer Configuration {{{
" Hide old buffers instead of close them when editing a new one. That is, allow
" the user to have unsaved changes in a buffer and still be able to open a new
" one.
set hidden

" Remember info about open buffers on close
set viminfo^=%

" Use ',bl' to list other buffers
nnoremap <leader>bl :CtrlPBuffer<CR>

" Use ',p' to switch between last two open buffers
map <leader>p :b#<CR>

" Next buffer
map gb :bnext<CR>

" Previous buffer
map gB :bprev<CR>

" Close the current buffer with ',db'
nmap <silent> <leader>bd :bd<CR>

" Close all the buffers with ',ba'
map <leader>ba :1,1000 bd!<cr>

" Jump to buffer number 1-9 with ',<N>' or 1-99 with '<N>gb'
let c = 1
while c <= 99
  if c < 10
    execute "nnoremap <silent> <leader>" . c . " :" . c . "b<CR>"
  endif
  execute "nnoremap <silent> " . c . "gb :" . c . "b<CR>"
  let c += 1
endwhile

" Specify the behavior when switching between buffers
try
    set switchbuf=useopen,usetab,newtab
    set stal=2
catch
endtry
" }}}

" Custom Functions {{{
" Return the number of open buffers
function! NrBuffs()
    let currentBufferNumber = bufnr('$')
    let numberOfBuffers = 0
    while currentBufferNumber >= 1
        if buflisted(currentBufferNumber)
            let numberOfBuffers+=1
        endif
        let currentBufferNumber-=1
    endwhile
    return numberOfBuffers
endfunction

" toggle between number and relativenumber
function! ToggleNumber()
    if(&relativenumber == 1)
        set norelativenumber
        set number
    else
        set relativenumber
    endif
endfunction

function! SourceConfigs()
  let l:pluginConfigDir = '~/.vim/plugin'
  let l:afterPluginConfigDir = '~/.vim/after/plugin'

  for f in split(glob(l:pluginConfigDir . '/*.vim'), '\n')
    exe 'source' f
  endfor
  for f in split(glob(l:afterPluginConfigDir . '/*.vim'), '\n')
    exe 'source' f
  endfor
endfunction

" Used to search for visually selected text
function! s:GetSelectedText()
  let l:old_reg = getreg('"')
  let l:old_regtype = getregtype('"')
  norm gvy
  let l:ret = getreg('"')
  call setreg('"', l:old_reg, l:old_regtype)
  exe "norm \<Esc>"
  return l:ret
endfunction
" }}}

" Autocommands {{{
" EVERY auto command in this section should be inside of the enclosing 'if'
" statement to avoid compatibility issues with versions of Vim that do not have
" autocmd functions.
if has('autocmd')
    " Make line numbers absolute upon entering insert mode, and
    " make them relative upon leaving insert mode.
    autocmd InsertEnter * :set number norelativenumber
    autocmd InsertLeave * :set number relativenumber

    " Enable file type detection
    filetype on

    " Treat .json files as .js
    autocmd BufNewFile,BufRead *.json setfiletype json syntax=javascript

    " Treat .md files as Markdown
    autocmd BufNewFile,BufRead *.md setlocal filetype=markdown

    " Strip trailing whitespace on buffer write
    autocmd BufWritePre * :%s/\s+$//e

    " Move cursor to last known position when reopening a file
    autocmd BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$")
          \| exe "normal! g'\"" | endif
endif
" }}}

" Sudo {{{
" Use ':w!!' to save changes to a file requiring root privileges that you opened
" without first invoking the 'sudo' command
cmap w!! w !sudo tee % > /dev/null
" }}}

" tmux {{{
" Change cursor from block cursor mode to vertical bar cursor mode in tmux
if exists('$TMUX')
    let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
    let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
else
    let &t_SI = "\<Esc>]50;CursorShape=1\x7"
    let &t_EI = "\<Esc>]50;CursorShape=0\x7"
endif
" }}}

" Visual Mode {{{
" Pressing * or # searches for the current visual selection
vnoremap <silent> * :call setreg("/",
    \ substitute(<SID>GetSelectedText(),
    \ '\_s\+',
    \ '\\_s\\+', 'g')
    \ )<Cr>n

vnoremap <silent> # :call setreg("?",
    \ substitute(<SID>GetSelectedText(),
    \ '\_s\+',
    \ '\\_s\\+', 'g')
    \ )<Cr>n
" }}}

" Color and Theme (should be last) {{{
if (has("termguicolors"))
  set termguicolors

  " Enable italics
  " Not necessary in Neovim
  " let &t_ZH = "\e[3m"
  " let &t_ZR = "\e[23m"
endif

" Set the theme here
let s:selected_theme = 'tne'

" Tomorrow Night Eighties {{{
if s:selected_theme == 'tne'
  syntax on
  colorscheme Tomorrow-Night-Eighties

  " Needed to highlight the cursor's line
  set cursorline

  " Make sure background is transparent if terminal is transparent.
  " You can turn this on if you want, but I prefer a solid background for my
  " editor.
  " hi Normal guibg=NONE
endif
" }}}

" Purify {{{
if s:selected_theme == 'purify'
  syntax on
  colorscheme purify

  " Needed to highlight the cursor's line
  set cursorline

  " Turn off cursor underline
  " (must specify all other options on for this to work)
  let g:purify_bold = 1
  let g:purify_italic = 1
  let g:purify_underline = 0
  let g:purify_undercurl = 1
  let g:purify_inverse = 1

  " Make sure that git-gutter symbols are colored correctly.
  " This is definitely a bug in the theme, and may eventually be fixed.
  " (pull request?)
  hi DiffAdd    guifg=#5FFF87 guibg=#313440 ctermfg=84  ctermbg=235
  hi DiffChange guifg=#FFFF87 guibg=#313440 ctermfg=228 ctermbg=235
  hi DiffDelete guifg=#FF0000 guibg=#313440 ctermfg=196 ctermbg=235
  hi DiffText   guifg=#FF79C6 guibg=#313440 ctermfg=212 ctermbg=235

  " Add a faint highlight for the line that the cursor is on.
  " Also get rid of the uncderline on the current line.
  hi CursorLine  guifg=NONE guibg=#3E4452 ctermbg=237
        \ gui=NONE term=NONE cterm=NONE
  " Change color of the colorcolumn to match the current line highlight
  hi ColorColumn guifg=NONE guibg=#3E4452 ctermbg=237
        \ gui=NONE term=NONE cterm=NONE
endif
" }}}

" Make comments italic
hi Comment gui=italic cterm=italic
" }}}

" vim:foldenable:foldmethod=marker
