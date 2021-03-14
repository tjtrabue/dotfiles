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

" Pasting from clipboard into vim is formatted correctly (,p)
map <silent> <leader>p :set paste<CR>"*p:set nopaste<CR>

" Quickly edit/reload the ~/.vimrc file
" ',ev' opens ~/.vimrc in a new buffer to edit.
" ',sv' sources the ~/.vimrc file.
map <silent> <leader>ev :e $MYVIMRC<CR>
map <silent> <leader>sv :so $MYVIMRC<CR>:call SourceConfigs()<CR>

" Switch CWD to the directory of the open buffer
map <leader>cd :cd %:p:h<cr>:pwd<cr>

" vim:foldenable:foldmethod=marker
