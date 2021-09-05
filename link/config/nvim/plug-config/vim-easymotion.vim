" Functions with 'overwin' in the name can move the cursor across windows.
" They only work in Normal mode, though.

" s{char}{char} to move to {char}{char}
nmap s <Plug>(easymotion-overwin-f2)

" Remap 'L' in normal mode to be more useful.
" Move to line
map L <Plug>(easymotion-bd-jk)
nmap L <Plug>(easymotion-overwin-line)

" Remap 'f' and 'F' in normal mode to be more useful.
" Move to {char}
map f <Plug>(easymotion-bd-f)
nmap f <Plug>(easymotion-overwin-f)
" Move to word
map F <Plug>(easymotion-bd-w)
nmap F <Plug>(easymotion-overwin-w)
