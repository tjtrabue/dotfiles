" which-key pops up a small buffer at the bottom of the screen when the leader
" key is pressed. The buffer contains a list of all keys and prefixes that can
" be activated through the leader key.

" Set the timeout until which-key kick's in.
set timeoutlen=500

" These are necessary for which-key to function.
let g:mapleader = ','
let g:maplocalleader = ','

" Remap the leader key to which-key's leader trigger.
nnoremap <silent> <leader>      :<c-u>WhichKey ','<CR>
nnoremap <silent> <localleader> :<c-u>WhichKey ','<CR>

call which_key#register(',', 'g:which_key_map')

" Define prefix dictionary
let g:which_key_map =  {}

" prefix dictionaries {{{

" Git {{{
let g:which_key_map.g = {
      \ 'name': '+git',
      \ 'g': ['LazyGit', 'lazy-git']
      \ }
" }}}

" LSP {{{
let g:which_key_map.l = {
      \ 'name': '+lsp'
      \ }
" }}}

" NvimTree {{{
let g:which_key_map.n = {
      \ 'name': '+nvim-tree',
      \ 't': ['NvimTreeToggle', 'nvim-tree-toggle'],
      \ 'r': ['NvimTreeRefresh', 'nvim-tree-refresh'],
      \ 'f': ['NvimTreeFindFile', 'nvim-tree-find-file'],
      \ }

" Telescope {{{
let g:which_key_map.f = {
      \ 'name': '+telescope',
      \ }
" }}}
" }}}

" vim:foldenable:foldmethod=marker:foldlevel=0
