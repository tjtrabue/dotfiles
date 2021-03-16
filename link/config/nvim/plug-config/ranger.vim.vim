" Do not add default ranger.vim keybindings.
let g:ranger_map_keys = 0

" Add custom keybindings for ranger.vim
nnoremap <leader>r :Ranger<CR>
" Mimic shell keybinding for ranger.
nnoremap <C-o> :Ranger<CR>
