" This file mainly contains keybindings for Telescope to use within Vim proper.

" Files {{{
nnoremap <leader>ff <cmd>lua require('telescope.builtin').find_files()<cr>
nnoremap <C-x>f <cmd>lua require('telescope.builtin').find_files()<cr>
nnoremap <leader>fs <cmd>lua require('telescope.builtin').live_grep()<cr>
nnoremap <C-p> <cmd>lua require('telescope.builtin').git_files()<cr>
" }}}

" Vim {{{
nnoremap <leader>fb <cmd>lua require('telescope.builtin').buffers()<cr>
nnoremap <C-x>b <cmd>lua require('telescope.builtin').buffers()<cr>
nnoremap <leader>fh <cmd>lua require('telescope.builtin').help_tags()<cr>
nnoremap <leader>fm <cmd>lua require('telescope.builtin').man_pages()<cr>
" }}}

" Git {{{
nnoremap <leader>fgc <cmd>lua require('telescope.builtin').git_commits()<cr>
nnoremap <leader>fgB <cmd>lua require('telescope.builtin').git_bcommits()<cr>
nnoremap <leader>fgb <cmd>lua require('telescope.builtin').git_branches()<cr>
nnoremap <leader>fgs <cmd>lua require('telescope.builtin').git_status()<cr>
" }}}

" LSP {{{
nnoremap <leader>fla <cmd>lua require('telescope.builtin').lsp_code_actions()<cr>
nnoremap <leader>fll <cmd>lua require('telescope.builtin').lsp_workspace_symbols()<cr>
nnoremap <leader>flr <cmd>lua require('telescope.builtin').lsp_references()<cr>
" }}}

" Lists {{{
nnoremap <leader>fLp <cmd>lua require('telescope.builtin').planets()<cr>
nnoremap <leader>fLs <cmd>lua require('telescope.builtin').symbols()<cr>
" }}}

" Themes {{{
nnoremap <leader>fT :lua require'telescope.builtin'.find_files(require('telescope.themes').get_dropdown({}))<cr>
" }}}

" vim:foldenable:foldmethod=marker:foldlevel=0
