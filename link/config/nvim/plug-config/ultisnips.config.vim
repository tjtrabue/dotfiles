" Remap expand trigger for snippets to Ctrl+E to avoid conflict with
" YouCompleteMe trigger (Tab)
let g:UltiSnipsExpandTrigger="<C-e>"

" Jump forward with Ctrl+L and backward with Ctrl+K
let g:UltiSnipsJumpForwardTrigger="<C-l>"
let g:UltiSnipsJumpBackwardTrigger="<C-k>"

" Tell UltiSnips where to locate and create private snippet files
let g:UltiSnipsSnippetsDir="$HOME/.vim/UltiSnips"
