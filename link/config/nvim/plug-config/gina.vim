" Basic mappings {{{
nnoremap <leader>ga :Gina! add --all<CR>
nnoremap <leader>gb :Gina branch -vv<CR>
nnoremap <leader>gB :Gina blame<CR>
nnoremap <leader>gc :Gina commit<CR>
nnoremap <leader>gC :Gina chaperon<CR>
nnoremap <leader>gd :Gina diff<CR>
nnoremap <leader>gl :Gina log<CR>
nnoremap <leader>gp :Gina push origin HEAD<CR>
nnoremap <leader>gP :Gina pull<CR>
nnoremap <leader>gs :Gina status -s<CR>
nnoremap <leader>gt :Gina tag<CR>
" }}}

" From sample config in help docs {{{
call gina#custom#command#alias('branch', 'br')
call gina#custom#command#option('br', '-v', 'v')

" Open interactive non-file-like Gina buffers in horizontal split
call gina#custom#command#option(
      \ '/\%(branch\|commit\|status\|tag\)',
      \ '--opener', '25split'
      \)

call gina#custom#command#option(
      \ '/\%(log\|reflog\)',
      \ '--opener', 'vsplit'
      \)
call gina#custom#command#option(
      \ 'log', '--group', 'log-viewer'
      \)
call gina#custom#command#option(
      \ 'reflog', '--group', 'reflog-viewer'
      \)
call gina#custom#command#option(
      \ 'commit', '-v|--verbose'
      \)
call gina#custom#command#option(
      \ 'status', '--ignore-submodules'
      \)
call gina#custom#command#option(
      \ '/\%(status\|commit\)',
      \ '-u|--untracked-files'
      \)
call gina#custom#command#option(
      \ '/\%(status\|changes\)',
      \ '--ignore-submodules'
      \)

call gina#custom#action#alias(
      \ 'branch', 'track',
      \ 'checkout:track'
      \)
call gina#custom#action#alias(
      \ 'branch', 'merge',
      \ 'commit:merge'
      \)
call gina#custom#action#alias(
      \ 'branch', 'rebase',
      \ 'commit:rebase'
      \)

call gina#custom#mapping#nmap(
      \ 'branch', 'g<CR>',
      \ '<Plug>(gina-commit-checkout-track)'
      \)

" Use Ctrl+f to navigate between Gina status and commit buffers
call gina#custom#mapping#nmap(
      \ 'status', '<C-f>',
      \ ':q<CR>:<C-u>Gina commit<CR>',
      \ {'noremap': 1, 'silent': 1}
      \)
call gina#custom#mapping#nmap(
      \ 'commit', '<C-f>',
      \ ':q<CR>:<C-u>Gina status -s<CR>',
      \ {'noremap': 1, 'silent': 1}
      \)

call gina#custom#execute(
      \ '/\%(status\|branch\|ls\|grep\|changes\|tag\)',
      \ 'setlocal winfixheight',
      \)
" }}}

" Modeline for this file (LEAVE IT COMMENTED!)
" vim:foldmethod=marker