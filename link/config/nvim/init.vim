" Neovim config file

" Redefine the user's vimrc' file to mean this file (~/.config/nvim/init.vim).
let $MYVIMRC = stdpath('config') . '/init.vim'

" Directory containing extra configuration files for plugins installed with
" vim-plug. We decide when and how to load these files. Neovim does not load
" them automatically.
let g:plugin_config_dir = stdpath('config') . '/plug-config'

" Directory containing '.vim' and '.lua' files that should override default
" config in the 'plugin/' and 'plug-config/' directories.
" This directory is not tracked by Git, and may or may not exist.
let g:override_config_dir = stdpath('config') . '/override-config'

" Directory containing all installed plugins
let g:plugin_install_dir = stdpath('data') . '/site/pack/packer/start'

" The file specifying all third-party plugins to use.
let g:packer_plugins_file = stdpath('config') . '/lua/plugins.lua'

" Settings {{{

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

" Enable code folding.
" NOTE: See autocmds.vim for foldenable logic. We only want to enable folding
" for large files, so we have to do that with autocmds.
set foldenable

" The number of fold occurrences after which Vim will start folding code.
set foldlevel=0

" The maximum number of folds to nest within one another.
set foldnestmax=1

" Default fold method to use.
" NOTE: See also the 'autocmds.vim' file for more fold settings.
set foldmethod=syntax
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

" Enable 24-bit RGB color in the TUI
set termguicolors

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
" }}}

" Functions {{{

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

" Mapleader {{{
" Change mapleader
let mapleader=","

" Save a file as root (,sr)
map <leader>sr :w !sudo tee % > /dev/null<CR>

" Use ',t' to toggle absolute and relative line numbers
map <leader>t :call ToggleNumber()<CR>

" Pasting from clipboard into vim is formatted correctly (,p)
map <silent> <leader>p :set paste<CR>"*p:set nopaste<CR>

" Quickly edit/reload the ~/.vimrc file
" ',ev' opens ~/.vimrc in a new buffer to edit.
" ',sv' sources the ~/.vimrc file.
map <silent> <leader>ev :e $MYVIMRC<CR>
map <silent> <leader>sv :so $MYVIMRC<CR>:call SourceConfigs()<CR>

" Switch CWD to the directory of the open buffer
map <leader>cd :cd %:p:h<cr>:pwd<CR>

" Update all plugins
nmap <leader>pu :PackerUpdate<CR>
" }}}

" Keybindings {{{

" Disable arrow keys {{{
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
" }}}
"
" Normal Mode Mappings {{{

" Move up and down by visual line, not actual line.
" i.e., if a line gets visually wrapped due to your text wrapping setting, j will not skip
" over the "fake" part of the visual line in favor of the "real" one.
nnoremap j gj
nnoremap k gk

" Highlight last inserted text
nnoremap gV `[v`]

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

" Buffer Mappings {{{

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
"
" File Tab Mappings {{{
"
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
"
" Sudo {{{
"
" Use ':w!!' to save changes to a file requiring root privileges that you opened
" without first invoking the 'sudo' command
cmap w!! w !sudo tee % > /dev/null
" }}}
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

" autocmds {{{

" Autocommands are Vim/Neovim rules for automatically running certain functions
" or commands based on certain events.

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

" Add syntax highlighting for additional filetypes {{{

" Interpret config files for IDE Vim plugins as Vim syntax
autocmd BufNewFile,BufRead .vrapperrc set filetype=vim syntax=vim
autocmd BufNewFile,BufRead .ideavimrc set filetype=vim syntax=vim

" Observe new Git config file locations and naming conventions.
autocmd BufNewFile,BufRead */git/ignore set filetype=gitignore syntax=gitignore
" }}}

" Highlight yanked text {{{
augroup highlight_yank
  autocmd!
  autocmd TextYankPost * silent!
        \lua require'vim.highlight'.on_yank{higroup="IncSearch", timeout=500}
augroup END
" }}}

" Code folding {{{

" We only want to initially fold all code in a buffer if the number of lines in
" the buffer exceeds a certain threshold. Otherwise, unfold all folds in the
" buffer.
"
" `winheight(0)` returns the height of the current window.
augroup fold_code
  autocmd BufRead * if line('$') >= winheight(0)
        \| silent execute "normal zM"
        \| else
        \| silent execute "normal zR"
        \| endif
augroup END
" }}}
" }}}

" Load plugins defined in ./lua/plugins.lua:
lua require('plugins')

" Private functions {{{

" Source a file in the plugin-config dir.
function! s:SourcePluginConfigFile(filename)
  let l:filepath = g:plugin_config_dir . '/' . a:filename

  if l:filepath =~ '\.vim$'
    silent execute 'source ' . l:filepath
  elseif l:filepath =~ '\.lua$'
    silent execute 'luafile ' . l:filepath
  endif
endfunction

" Source all files in the 'override-config/' directory. The 'override-config/'
" directory does not have to be present for this function to work properly, and
" is only intended to allow for machine-local overrides of our baseline
" configuration.
function! s:SourceOverrideConfigFiles()
  for f in split(glob(g:override_config_dir . '/*'), '\n')
    if f =~ '\.vim$'
      silent execute 'source ' . f
    elseif f =~ '\.lua$'
      silent execute 'luafile ' . f
    endif
  endfor
endfunction
" }}}

" Source plugin configuration files. {{{
"
" We don't want to keep these files in the standard 'plugin/' directory because
" those files are not guaranteed to load in a given order, and may even load
" asynchronously. We want to control when and where we load these plugin
" configuration files so that we don't encounter strange errors when Neovim
" first loads.

" This should be first, since which key ought to be registered before we make
" any leader mappings.
call s:SourcePluginConfigFile('vim-which-key.config.vim')

call s:SourcePluginConfigFile('airline.config.vim')
call s:SourcePluginConfigFile('awesome_terminal_fonts.config.vim')
call s:SourcePluginConfigFile('ctrlp.config.vim')
call s:SourcePluginConfigFile('emmet.config.vim')
call s:SourcePluginConfigFile('vim-signify.config.vim')
call s:SourcePluginConfigFile('gundo.config.vim')
call s:SourcePluginConfigFile('incsearch-easymotion.config.vim')
call s:SourcePluginConfigFile('incsearch.config.vim')
call s:SourcePluginConfigFile('latex.config.vim')
call s:SourcePluginConfigFile('tagbar.config.vim')
call s:SourcePluginConfigFile('tmuxline.config.vim')
call s:SourcePluginConfigFile('ultisnips.config.vim')
call s:SourcePluginConfigFile('vim-easymotion.config.vim')
call s:SourcePluginConfigFile('vim_latex_live_preview.config.vim')
call s:SourcePluginConfigFile('lazygit.config.vim')
call s:SourcePluginConfigFile('undotree.config.vim')
call s:SourcePluginConfigFile('neogit.config.lua')
call s:SourcePluginConfigFile('neogit.config.vim')
call s:SourcePluginConfigFile('git-blame.config.vim')
call s:SourcePluginConfigFile('glow.config.vim')
call s:SourcePluginConfigFile('barbar.config.vim')
call s:SourcePluginConfigFile('nvim-hlslens.config.vim')
call s:SourcePluginConfigFile('formatter.config.lua')
call s:SourcePluginConfigFile('ranger.config.vim')

call s:SourcePluginConfigFile('nvim-tree.config.lua')
" call s:SourcePluginConfigFile('chadtree.config.vim')

call s:SourcePluginConfigFile('indent-guides.config.lua')
" Currently not using fzf. Using telescope.nvim instead.
" call s:SourcePluginConfigFile('fzf.config.vim')
call s:SourcePluginConfigFile('telescope.config.lua')
call s:SourcePluginConfigFile('telescope.config.vim')

" Load auto-completion configuration
call s:SourcePluginConfigFile('nvim-cmp.config.lua')

" Load Aerial code browser config file
call s:SourcePluginConfigFile('aerial.config.lua')

" Load the LSP configuration file.
call s:SourcePluginConfigFile('lsp.config.lua')

" Better defaults for nvim-lsp.
call s:SourcePluginConfigFile('nvim-lsputils.config.lua')

" Trouble is a pretty LSP diagnostic browser for Neovim.
call s:SourcePluginConfigFile('trouble.config.lua')

" Define new textobjects with the help of treesitter and LSP.
call s:SourcePluginConfigFile('treesitter-textobjs.config.lua')

" Define treesitter configuration
call s:SourcePluginConfigFile('nvim-treesitter.conf.config.lua')

" Highlight colors in files for color names, RGB values, and hex codes.
call s:SourcePluginConfigFile('nvim-colorizer.config.lua')

" These should always come last!!!
call s:SourcePluginConfigFile('colorscheme.config.vim')
call s:SourcePluginConfigFile('nvim-web-devicons.config.lua')

" Activate the awesome galaxyline statusbar. This status line must be entirely
" self-configured. That is, there is no 'default' status line that comes with
" galaxyline. However, the repo does contain a few example files that you can
" use as a starting point for your own custom status lines.
call s:SourcePluginConfigFile('galaxyline.config.vim')

" lualine is a fast, minimal framework for building statuslines.
" call s:SourcePluginConfigFile('lualine.config.lua')

" Org mode configuration
call s:SourcePluginConfigFile('orgmode.config.lua')

" Code linter that works alongside Neovim's LSP.
call s:SourcePluginConfigFile('nvim-lint.config.lua')

" PostgreSQL syntax highlighting config
call s:SourcePluginConfigFile('pgsql.config.vim')
" }}}

" Source override config files {{{
call s:SourceOverrideConfigFiles()
" }}}

" Automatically compile new plugins whenever the plugins.lua file is
" modified.
autocmd BufWritePost g:packer_plugins_file PackerCompile

" vim:foldenable:foldmethod=marker:foldlevel=0
