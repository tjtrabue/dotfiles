" This file configures completion-nvim, a better completion front-end for
" nvim-lsp, the built-in LSP client for Neovim.

" Settings {{{

" Specify the snippet engine to use.
let g:completion_enable_snippet = 'UltiSnips'

" Map the key to use for confirming the completion selection.
" <CR> by default.
" let g:completion_confirm_key = "\<C-l>"
" Settings the confirm function in the Keymappings section.
let g:completion_confirm_key = ""
" }}}

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

" Keymappings {{{
" Complete the current selection
imap <expr> <C-l>  pumvisible() ? complete_info()["selected"] != "-1" ?
                 \ "\<Plug>(completion_confirm_completion)" : "\<C-e>\<CR>" : "\<CR>"

" Use <Tab> and <S-Tab> to navigate through popup menu
inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

" Use <C-j> and <C-k> to navigate through popup menu
inoremap <expr> <C-j>   pumvisible() ? "\<C-n>" : "\<C-j>"
inoremap <expr> <C-k> pumvisible() ? "\<C-p>" : "\<C-k>"

" Set completeopt to have a better completion experience
set completeopt=menuone,noinsert,noselect

" Avoid showing message extra message when using completion
set shortmess+=c
" }}}

" vim:foldenable:foldmethod=marker:foldlevel=0
