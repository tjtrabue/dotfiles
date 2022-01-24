" NOTE: You must have the lazygit system program installed before you can
" use it with Neovim.

let g:lazygit_floating_window_winblend = 0 " transparency of floating window
let g:lazygit_floating_window_scaling_factor = 0.9 " scaling factor for floating window
let g:lazygit_floating_window_corner_chars = ['╭', '╮', '╰', '╯'] " customize lazygit popup window corner characters
let g:lazygit_use_neovim_remote = 1 " fallback to 0 if neovim-remote is not installed

" setup mapping to call :LazyGit
nnoremap <silent> <leader>lg :LazyGit<CR>

" vim:foldenable:foldmethod=marker:foldlevel=0
