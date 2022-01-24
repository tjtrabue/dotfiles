" Customize Neovim's colorscheme here.
" For Neovim 0.1.3 and 0.1.4 - https://github.com/neovim/neovim/pull/2198
let $NVIM_TUI_ENABLE_TRUE_COLOR = 1

" Variable controlling which theme we want to use.
let s:selected_theme = 'neon'

" For Neovim > 0.1.5 and Vim > patch 7.4.1799 - https://github.com/vim/vim/commit/61be73bb0f965a895bfb064ea3e55476ac175162
" Based on Vim patch 7.4.1770 (`guicolors` option) - https://github.com/vim/vim/commit/8a633e3427b47286869aa4b96f2bfc1fe65b25cd
" https://github.com/neovim/neovim/wiki/Following-HEAD#20160511
if (has('termguicolors'))
  set termguicolors

  " Enable italics
  " Not necessary in Neovim
  " let &t_ZH = "\e[3m"
  " let &t_ZR = "\e[23m"
endif

" Material {{{
if s:selected_theme == 'material'
  " Can be one of: 'default', 'palenight', 'ocean', 'lighter', 'darker',
  " 'default-community', 'palenight-community', 'ocean-community',
  " 'lighter-community', 'darker-community'
  let g:material_theme_style = 'darker'

  colorscheme material
endif
" }}}

" onehalf {{{
if s:selected_theme == 'onehalf'
  colorscheme onehalfdark
  let g:airline_theme='onehalfdark'
endif
" }}}

" Tomorrow Night Eighties {{{
if s:selected_theme == 'tne'
  syntax on
  colorscheme Tomorrow-Night-Eighties

  " Make sure background is transparent if terminal is transparent.
  " You can turn this on if you want, but I prefer a solid background for my
  " editor.
  " hi Normal guibg=NONE
endif
" }}}

" Purify {{{
if s:selected_theme == 'purify'
  syntax on
  colorscheme purify

  " Turn off cursor underline
  " (must specify all other options on for this to work)
  let g:purify_bold = 1
  let g:purify_italic = 1
  let g:purify_underline = 0
  let g:purify_undercurl = 1
  let g:purify_inverse = 1

  " Make sure that git-gutter symbols are colored correctly.
  " This is definitely a bug in the theme, and may eventually be fixed.
  " (pull request?)
  hi DiffAdd    guifg=#5FFF87 guibg=#313440 ctermfg=84  ctermbg=235
  hi DiffChange guifg=#FFFF87 guibg=#313440 ctermfg=228 ctermbg=235
  hi DiffDelete guifg=#FF0000 guibg=#313440 ctermfg=196 ctermbg=235
  hi DiffText   guifg=#FF79C6 guibg=#313440 ctermfg=212 ctermbg=235

  " Add a faint highlight for the line that the cursor is on.
  " Also get rid of the uncderline on the current line.
  hi CursorLine  guifg=NONE guibg=#3E4452 ctermbg=237
        \ gui=NONE term=NONE cterm=NONE
  " Change color of the colorcolumn to match the current line highlight
  hi ColorColumn guifg=NONE guibg=#3E4452 ctermbg=237
        \ gui=NONE term=NONE cterm=NONE
endif
" }}}

" Sonokai {{{
if s:selected_theme == 'sonokai'
  " Can be 'default', 'andromeda', 'atlantis', 'maia', or 'shusia'.
  let g:sonokai_style = 'default'
  " Must manually enable italic support.
  let g:sonokai_enable_italic = 1
  colorscheme sonokai
endif
" }}}

" Neon {{{
if s:selected_theme == 'neon'
  " Which color style to use
  let g:neon_style = 'doom'

  " Italic text for comment
  let g:neon_italic_comment = 1
  " Applies to conditionals and keywords
  let g:neon_italic_keyword = 1
  " true and false
  let g:neon_italic_boolean = 1
  " Applies to function (calls and definitions), methods (calls and
  " definitions), and built-in functions.
  let g:neon_italic_function = 1
  " Applies to variable names defined by languages, like 'this' or 'self'.
  let g:neon_italic_variable = 1
  " Applies to error and warning messages, functions, lsp virtual text, etc.
  let g:neon_bold = 1
  " Applies no background color to certain highlight groups.
  let g:neon_transparent = 0

  colorscheme neon
endif
" }}}

" Make comments italic
hi Comment gui=italic cterm=italic

" vim:foldenable:foldmethod=marker:foldlevel=0
