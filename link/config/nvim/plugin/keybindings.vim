" Keybindings file for Neovim

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

" vim:foldenable:foldmethod=marker:foldlevel=0
