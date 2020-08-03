" Order matching files top to bottom
let g:ctrlp_match_window = 'bottom,order:ttb'

" Always open files in new buffers
let g:ctrlp_switch_buffer = 0

" Allow the user to change working directories in Vim and have CtrlP respect that change
let g:ctrlp_working_path_mode = 0

" Exclude version control directories when searching
set wildignore+=*/.git/*,*/.hg/*,*/.svn/*

" Use ag (The Silver Searcher) to greatly speed up file searching
let g:ctrlp_user_command = 'ag -l -p "$HOME/.agignore" --nocolor --hidden -g "" %s | sort'
"let g:ctrlp_user_command = {
"    \ 'types': {
"      \ 1: ['.git', 'cd %s && git ls-files'],
"      \ 2: ['.hg', 'hg --cwd %s locate -I .'],
"      \ },
"    \ 'fallback': 'ag -l -p "$HOME/.agignore" --nocolor --hidden -g "" %s | sort',
"    \ 'ignore': 1
"    \ }

" When pressing <C-o>, open multiple files in hidden buffers
let g:ctrlp_open_multiple_files = 'i'

" Don't prompt user for how to open multiple files
let g:ctrlp_arg_map = 0
