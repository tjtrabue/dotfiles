" Keybindings file for Neovim

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
"
" vim:foldenable:foldmethod=marker:foldlevel=0
