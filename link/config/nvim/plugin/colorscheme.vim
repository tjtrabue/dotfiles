" Customize Neovim's colorscheme here.
" For Neovim 0.1.3 and 0.1.4 - https://github.com/neovim/neovim/pull/2198
let $NVIM_TUI_ENABLE_TRUE_COLOR = 1

" For Neovim > 0.1.5 and Vim > patch 7.4.1799 - https://github.com/vim/vim/commit/61be73bb0f965a895bfb064ea3e55476ac175162
" Based on Vim patch 7.4.1770 (`guicolors` option) - https://github.com/vim/vim/commit/8a633e3427b47286869aa4b96f2bfc1fe65b25cd
" https://github.com/neovim/neovim/wiki/Following-HEAD#20160511
if (has('termguicolors'))
  set termguicolors
endif

" Can be one of: 'default', 'palenight', 'ocean', 'lighter', 'darker',
" 'default-community', 'palenight-community', 'ocean-community',
" 'lighter-community', 'darker-community'
let g:material_theme_style = 'darker'

colorscheme material
