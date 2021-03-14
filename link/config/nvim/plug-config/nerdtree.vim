" Show dotfiles in NERDTree
let NERDTreeShowHidden = 1

" Any autocmds should go inside of this 'if' statement
if has('autocmd')
    " Uncomment this line to start NERDTree when vim opens
    "autocmd vimenter * NERDTree

    " Start NERDTree when vim opens even if no files were specified
    " autocmd StdinReadPre * let s:std_in=1
    " autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif

    " Put the focus on the primary file when vim opens (instead of on NERDTree)
    " autocmd VimEnter * wincmd p

    augroup NERDTreeAutocommands
      " Open NERDTree when Vim starts if no files are opened
      autocmd StdinReadPre * let s:std_in=1
      autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif

      " Open NERDTree when Vim starts if a directory is opened
      autocmd StdinReadPre * let s:std_in=1
      autocmd VimEnter * if argc() == 1 &&
            \ isdirectory(argv()[0]) &&
            \ !exists("s:std_in") | exe 'NERDTree' argv()[0] | wincmd p | ene | exe 'cd '.argv()[0] | endif
      augroup END
endif

" Exit Vim if NERDTree is the last and only open buffer
function! s:CloseIfOnlyControlWinLeft()
    if winnr("$") != 1
        return
    endif
    if (exists("t:NERDTreeBufName") && bufwinnr(t:NERDTreeBufName) != -1
        \ && NrBuffs() == 1) || &buftype == 'quickfix'
        q
    endif
endfunction
augroup CloseIfOnlyControlWinLeft
    au!
    au BufEnter * call s:CloseIfOnlyControlWinLeft()
augroup END

" Use ',n' to toggle NERDTree
" If NERDTree is toggled on, then focus is automatically placed on previously addressed window
map <leader>n :NERDTreeToggle<CR><C-w>p
