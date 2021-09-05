" This file configures completion-nvim, a better completion front-end for
" nvim-lsp, the built-in LSP client for Neovim.

" Specify the snippet engine to use.
let g:completion_enable_snippet = 'UltiSnips'

" Chain complete {{{
lua <<EOF
vim.g.completion_chain_complete_list = {
  -- org mode completion
  org = {
    { mode = 'omni'},
  },
}
vim.cmd[[autocmd FileType org setlocal iskeyword+=:,#,+]]
EOF
" }}}

" vim:foldenable:foldmethod=marker:foldlevel=0
