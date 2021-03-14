" Enable smart tab line above file that lists all open buffers as long as only one tab is open
let g:airline#extensions#tabline#enabled = 1

" Set airline color theme
let g:airline_theme = 'tomorrow'

" Use Powerline font symbols:
let g:airline_powerline_fonts = 1

" Specify powerline symbols
" NOTE: Should be unnecessary! Check your font if you're having trouble displaying powerline symbols
" if !exists('g:airline_symbols')
"   let g:airline_symbols = {}
" endif
" let g:airline_left_sep = ''
" let g:airline_left_alt_sep = ''
" let g:airline_right_sep = ''
" let g:airline_right_alt_sep = ''
" let g:airline_symbols.branch = ''
" let g:airline_symbols.readonly = ''
" let g:airline_symbols.linenr = '☰'
" let g:airline_symbols.maxlinenr = ''
" let g:airline_symbols.dirty='⚡'
