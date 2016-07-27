" Use Pathogen
execute pathogen#infect()

" Use the Solarized Dark theme
set background=dark
colorscheme solarized
let g:solarized_termtrans=1

" Make Vim more useful
set nocompatible
" Use the OS clipboard by default (on versions compiled with `+clipboard`)
set clipboard=unnamed
" Enhance command-line completion
set wildmenu
" Allow cursor keys in insert mode
set esckeys
" Allow backspace in insert mode
set backspace=indent,eol,start
" Optimize for fast terminal connections
set ttyfast
" Add the g flag to search/replace by default
set gdefault
" Use UTF-8 without BOM
set encoding=utf-8 nobomb
" Change mapleader
let mapleader=","
" Don’t add empty newlines at the end of files
set binary
set noeol
" Centralize backups, swapfiles and undo history
set backupdir=~/.vim/backups
set directory=~/.vim/swaps
if exists("&undodir")
    set undodir=~/.vim/undo
endif

" Don't use soft tabs, so as to avoid compatibility errors with other editors
set expandtab
set tabstop=4
set softtabstop=4
set shiftwidth=4

" Use auto-indent and set line width to 100 characters
set autoindent
set textwidth=100

" Function to enter a distraction-free word processor mode
function! WordProcessorMode()
    setlocal textwidth=100
    setlocal smartindent
    setlocal spell spelllang=en_us
    setlocal noexpandtab
endfunction
" typing ',WP' in command mode enters word processor mode
noremap <leader>WP :call WordProcessorMode()<CR>

" Don’t create backups when editing files in certain directories
set backupskip=/tmp/*,/private/tmp/*

" Respect modeline in files
set modeline
set modelines=4
" Enable per-directory .vimrc files and disable unsafe commands in them
set exrc
set secure
" Enable line numbers
set number
" Enable syntax highlighting
syntax enable
" Highlight current line
set cursorline
" Show “invisible” characters
set lcs=tab:▸\ ,trail:·,eol:¬,nbsp:_
set list
" Highlight searches
set hlsearch
" Ignore case of searches
set ignorecase
" Highlight dynamically as pattern is typed
set incsearch
" Always show status line
set laststatus=2
" Enable mouse in all modes
set mouse=a
" Disable error bells
set noerrorbells
" Don’t reset cursor to start of line when moving around.
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
" Use relative line numbers
if exists("&relativenumber")
    set relativenumber
    au BufReadPost * set relativenumber
endif
" Start scrolling three lines before the horizontal window border
set scrolloff=3

" Strip trailing whitespace (,ss)
function! StripWhitespace()
    let save_cursor = getpos(".")
    let old_query = getreg('/')
    :%s/\s\+$//e
    call setpos('.', save_cursor)
    call setreg('/', old_query)
endfunction
noremap <leader>ss :call StripWhitespace()<CR>
" Save a file as root (,W)
noremap <leader>W :w !sudo tee % > /dev/null<CR>

" Use jk as an Esc alias to exit insert mode
:imap jk <Esc>

" Use ',.' to go to end of current line
noremap <leader>. $
" Use ',m' to go to beginning of current line
noremap <leader>m ^

" Use 'ZX' to exit without saving
noremap ZX :q!<CR>

" Pasting from clipboard into vim is formatted correctly (,p)
nmap <silent> <leader>p :set paste<CR>"*p:set nopaste<CR>

" Automatic commands
if has("autocmd")
    " Enable file type detection
    filetype on
    " Treat .json files as .js
    autocmd BufNewFile,BufRead *.json setfiletype json syntax=javascript
    " Treat .md files as Markdown
    autocmd BufNewFile,BufRead *.md setlocal filetype=markdown
endif

" Automatically close square brackets, braces, parentheses, angle brackets, and both kinds of
" quotation marks.
inoremap ( ()<Esc>i
inoremap [ []<Esc>i
inoremap { {<CR>}<Esc>O<TAB>
autocmd Syntax html,vim inoremap < <lt>><Esc>i| inoremap > <c-r>=ClosePair('>')<CR>
inoremap ) <c-r>=ClosePair(')')<CR>
inoremap ] <c-r>=ClosePair(']')<CR>
inoremap } <c-r>=CloseBracket()<CR>
inoremap " <c-r>=QuoteDelim('"')<CR>
inoremap ' <c-r>=QuoteDelim("'")<CR>

" Closes a pair of parentheses or square brackets when the user types a closing
" character and the symbol under the cursor is that same closing character.
" e.g., typing `]` while a ']' is under the cursor does not an extra ']', it only moves the cursor
" ahead.
function ClosePair(char)
    if getline('.')[col('.') - 1] == a:char
        return "\<Right>"
    else
        return a:char
    endif
endf

" Close a pair of braces if the user types a closing brace and the char under  the cursor is a
" closing brace, or the only char on the next line down from the cursor is a closing brace.
" e.g., typing `}` when a '}' is under the cursor or is the first symbol on the line below the
" current line will move the cursor to that next '}' without adding an extra one.
function CloseBracket()
    if match(getline(line('.') + 1), '\s*}') < 0
        return "\<CR>}"
    else
        return "\<Esc>j0f}a"
    endif
endf

" Close a pair of single or double quotes if the character under the cursor is the quotation mark typed
function QuoteDelim(char)
    let line = getline('.')
    let col = col('.')
    if line[col - 2] == "\\"
        " Inserting a quoted quotation mark into the string
        return a:char
    elseif line[col - 1] == a:char
        " Escaping out of the string
        return "\<Right>"
    else
        " Starting a string
        return a:char.a:char."\<Esc>i"
    endif
endf

" Jump out of brackets, braces, or quotation marks (Ctrl+m)
:inoremap <C-m> <Esc>/[)}"'\]>]<CR>:nohl<CR>a

" Let (Ctrl+h) and (Ctrl+l) move the cursor to the beginning and end of a line, respectively.
:inoremap <C-h> <C-o>^i
:inoremap <C-l> <C-o>$i
:noremap <C-h> ^
:noremap <C-l> $

" Let (Ctrl+j) and (Ctrl+k) move the cursor to the beginning and end of a line, respectively.
:inoremap <C-j> <C-o>>bi
:inoremap <C-k> <C-o>wii
:noremap <C-j> b
:noremap <C-k> w

" Delete word under cursor while in either normal or insert mode.
:inoremap <C-d> <C-o>diw
:noremap <C-d> daw

" Delete line while in either normal or insert mode.
:inoremap <C-a> <C-o>dd
:noremap <C-a> dd

python from powerline.vim import setup as powerline_setup
python powerline_setup()
python del powerline_setup