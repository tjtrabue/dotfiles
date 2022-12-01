" Functions with 'overwin' in the name can move the cursor across windows.
" They only work in Normal mode, though.

" Settings {{{
" smartcase
let g:EasyMotion_smartcase = 1
" smartsign
let g:EasyMotion_use_smartsign_us = 1 " US layout
" Keep cursor column when JK motion
let g:EasyMotion_startofline = 0
" }}}

" Keymappings {{{

" Remap 'f' and 'F' in normal mode to be more useful.
" Move to word starting with {char}
nmap f <Plug>(easymotion-s)
" Move to word
nmap F <Plug>(easymotion-overwin-w)

" s{char}{char} to move to {char}{char}
nmap s <Plug>(easymotion-overwin-f2)

" Jump till character
nmap t <Plug>(easymotion-bd-t)

" Remap 'L' in normal mode to be more useful.
" Move to line
nmap L <Plug>(easymotion-overwin-line)
" }}}

" vim:foldenable:foldmethod=marker:foldlevel=0
