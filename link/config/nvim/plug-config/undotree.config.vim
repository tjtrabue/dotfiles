
" Store undo info in a directory between Vim sessions.
if has("persistent_undo")
    set undodir=$HOME."/.undodir"
    set undofile
endif

nnoremap <leader>ut :UndotreeToggle<CR>

" vim:foldenable:foldmethod=marker:foldlevel=0
