" Custom Neovim functions.

" Return the number of open buffers
function! NrBuffs()
    let currentBufferNumber = bufnr('$')
    let numberOfBuffers = 0
    while currentBufferNumber >= 1
        if buflisted(currentBufferNumber)
            let numberOfBuffers+=1
        endif
        let currentBufferNumber-=1
    endwhile
    return numberOfBuffers
endfunction

" toggle between number and relativenumber
function! ToggleNumber()
    if(&relativenumber == 1)
        set norelativenumber
        set number
    else
        set relativenumber
    endif
endfunction

function! SourceConfigs()
  let l:pluginConfigDir = '~/.vim/plugin'
  let l:afterPluginConfigDir = '~/.vim/after/plugin'

  for f in split(glob(l:pluginConfigDir . '/*.vim'), '\n')
    exe 'source' f
  endfor
  for f in split(glob(l:afterPluginConfigDir . '/*.vim'), '\n')
    exe 'source' f
  endfor
endfunction

" Used to search for visually selected text
function! s:GetSelectedText()
  let l:old_reg = getreg('"')
  let l:old_regtype = getregtype('"')
  norm gvy
  let l:ret = getreg('"')
  call setreg('"', l:old_reg, l:old_regtype)
  exe "norm \<Esc>"
  return l:ret
endfunction
"
" vim:foldenable:foldmethod=marker:foldlevel=0
