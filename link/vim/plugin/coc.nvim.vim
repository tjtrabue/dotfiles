" Config file for coc.nvim

" Make the popup menu not select the first completion item, but rather just insert the longest
" common text of all matches, and make the menu come up even if there's only one match.
set completeopt=longest,menuone

" If hidden is not set, TextEdit might fail.
set hidden

" Some servers have issues with backup files
set nobackup
set nowritebackup

set signcolumn=yes

augroup CocAutocommands
  " Clear all autocommands in this group
  autocmd!

  " Detect comments in coc-settings.json file (which is a JSONC file)
  autocmd BufEnter,BufNewFile coc-settings.json syntax match Comment +\/\/.\+$+
  " Close the preview window when completion is done
  autocmd! CompleteDone * if pumvisible() == 0 | pclose | endif
  " Highlight symbol under cursor on CursorHold
  autocmd CursorHold * silent call CocActionAsync('highlight')
  " Setup formatexpr specified filetype(s)
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup END

augroup CocGroup
  " Add comment syntax highlighting to the coc-settings.json configuration file
  autocmd BufRead,BufNewFile coc-settings.json syntax match Comment +\/\/.\+$+
  " Close preview window when completion is done
  autocmd! CompleteDone * if pumvisible() == 0 | pclose | endif
augroup end
" }}}

" Improve completion experience {{{
" Use Ctrl-j and Ctrl-k to navigate completion options
inoremap <expr> <C-j> pumvisible() ? "\<C-n>" : "\<C-j>"
inoremap <expr> <C-k> pumvisible() ? "\<C-p>" : "\<C-k>"

" Use ENTER to confirm completion
" NOTE: Currently broken, for some reason. Inserts "pumvisible" and a bunch of garbage right now.
" inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
" }}}

" Mappings {{{
" Use <tab> for trigger completion and navigate to the next complete item
function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1] =~ '\s'
endfunction

inoremap <silent><expr> <Tab>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<Tab>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

" Use Ctrl+Space to trigger completion
inoremap <silent><expr> <C-space> coc#refresh()

" Use `[c` and `]c` to navigate diagnostics
nmap <silent> [c <Plug>(coc-diagnostic-prev)
nmap <silent> ]c <Plug>(coc-diagnostic-next)

" Remap keys for gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window
function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction
nnoremap <silent> K :call <SID>show_documentation()<CR>

" Remap for rename current word
nmap <leader>rn <Plug>(coc-rename)

" Remap for format selected region
xmap <leader>f <Plug>(coc-format-selected)
nmap <leader>f <Plug>(coc-format-selected)

" Remap for do codeAction of selected region, ex: `<leader>aap` for current paragraph
xmap <leader>a <Plug>(coc-codeaction-selected)
nmap <leader>a <Plug>(coc-codeaction-selected)

" Remap for do codeAction of current line
nmap <leader>ac <Plug>(coc-codeaction)

" Fix autofix problem of current line
nmap <leader>qf <Plug>(coc-fix-current)

" Use <tab> for select selections ranges, needs server support, like coc-tsserver,
" or coc-python
nmap <silent> <TAB> <Plug>(coc-range-select)
xmap <silent> <TAB> <Plug>(coc-range-select)
xmap <silent> <S-TAB> <Plug>(coc-range-select-backward)

nmap <silent> <leader>df <Plug>(coc-fix)
nmap <silent> <leader>dd <Plug>(coc-definition)
nmap <silent> <leader>dr <Plug>(coc-references)
nmap <silent> <leader>dj <Plug>(coc-implementation)
nmap <silent> <leader>dn <Plug>(coc-rename)

nnoremap <silent> <C-c>f :Format<CR>
" }}}

" Commands {{{
" Use `:Format` to format current buffer
command! -nargs=0 Format :call CocAction('format')

" Use `:Fold` to fold current buffer
command! -nargs=? Fold :call CocAction('fold', <f-args>)

" Use `:OR` to organize imports of current buffer
command! -nargs=0 OR :call CocAction('runCommand', 'editor.action.organizeImport')
" }}}

" Statusline {{{
" Add diagnostic info for lightline plugin (if used)
let g:lightline = {
  \ 'colorscheme': 'purify',
  \ 'active': {
  \    'left': [ [ 'mode', 'paste' ],
  \              ['cocstatus', 'readonly', 'filename', 'modified' ] ]
  \ },
  \ 'component_function': {
  \   'cocstatus': 'coc#status'
  \ },
  \ }

" Get diagnostic message
function! StatusDiagnostic() abort
  let info = get(b:, 'coc_diagnostic_info', {})

  if empty(info) | return '' | endif

  let msgs = []

  if get(info, 'error', 0)
    call add(msgs, 'E' . info['error'])
  endif

  if get(info, 'warning', 0)
    call add(msgs, 'W' . info['warning'])
  endif

  return join(msgs, ' ') . ' ' . get(g:, 'coc_status', '')
endfunction

set statusline^=%{StatusDiagnostic()}
" }}}

" Using CocList {{{
" Show all diagnostics
nnoremap <silent> <space>a :<C-u>CocList diagnostics<CR>
" Manage extensions
nnoremap <silent> <space>e :<C-u>CocList extensions<CR>
" Show commands
nnoremap <silent> <space>c :<C-u>CocList commands<CR>
" Find symbol of current document
nnoremap <silent> <space>o :<C-u>CocList outline<CR>
" Search workspace symbols
nnoremap <silent> <space>s :<C-u>CocList -I symbols<CR>
" Do default action for next item.
nnoremap <silent> <space>j :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent> <space>k :<C-u>CocPrev<CR>
" Resume latest COC list
nnoremap <silent> <space>p :<C-u>CocListResume<CR>

" Use Ctrl+c Ctrl+p for file searching feature using the 'files' list.
nnoremap <silent> <C-c><C-p> :<C-u>CocList files<CR>
" Also remap good old Ctrl+p to file search capability
nnoremap <silent> <C-p> :<C-u>CocList files<CR>
" Use Ctrl+c Ctrl+b to list open buffers
nnoremap <silent> <C-c><C-b> :<C-u>CocList buffers<CR>
" Use Ctrl+c Ctrl+b to list Vim helptags
nnoremap <silent> <C-c><C-h> :<C-u>CocList helptags<CR>
" Use Ctrl+c Ctrl+g to search text in the current workspace
nnoremap <silent> <C-c><C-g> :<C-u>CocList grep<CR>
" }}}

" coc-smartf {{{
" This extension makes finding characters more obvious
nmap f <Plug>(coc-smartf-forward)
nmap F <Plug>(coc-smartf-backward)
nmap ; <Plug>(coc-smartf-repeat)
" }}}

" coc-explorer {{{
" Cool project explorer similar to that of many IDEs
nnoremap ge :CocCommand explorer<CR>
" }}}

" Modeline for this file (LEAVE IT COMMENTED!)
" vim:foldmethod=marker