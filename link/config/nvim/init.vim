" Neovim config file

" Redefine the user's vimrc' file to mean this file (~/.config/nvim/init.vim).
let $MYVIMRC = stdpath('config') . '/init.vim'

" Directory containing extra configuration files for plugins installed with
" vim-plug. We decide when and how to load these files. Neovim does not load
" them automatically.
let s:plugin_config_dir = stdpath('config') . '/plug-config'

" Install and activate plugins.
exec 'source ' . stdpath('config') . '/plugins.vim'

" Source plugin configuration files. {{{
"
" We don't want to keep these files in the standard 'plugin/' directory because
" those files are not guaranteed to load in a given order, and may even load
" asynchronously. We want to control when and where we load these plugin
" configuration files so that we don't encounter strange errors when Neovim
" first loads.

exec 'source ' . s:plugin_config_dir . '/airline.vim'
exec 'source ' . s:plugin_config_dir . '/awesome_terminal_fonts.vim'
exec 'source ' . s:plugin_config_dir . '/ctrlp.vim'
exec 'source ' . s:plugin_config_dir . '/emmet.vim'
exec 'source ' . s:plugin_config_dir . '/fugitive.vim'
exec 'source ' . s:plugin_config_dir . '/fzf.vim'
exec 'source ' . s:plugin_config_dir . '/gina.vim'
exec 'source ' . s:plugin_config_dir . '/gitgutter.vim'
exec 'source ' . s:plugin_config_dir . '/gundo.vim'
exec 'source ' . s:plugin_config_dir . '/incsearch-easymotion.vim'
exec 'source ' . s:plugin_config_dir . '/incsearch.vim'
exec 'source ' . s:plugin_config_dir . '/indent_guides.vim'
exec 'source ' . s:plugin_config_dir . '/latex.vim'
exec 'source ' . s:plugin_config_dir . '/nerdcommenter.vim'
exec 'source ' . s:plugin_config_dir . '/nerdtree.vim'
exec 'source ' . s:plugin_config_dir . '/tagbar.vim'
exec 'source ' . s:plugin_config_dir . '/tmuxline.vim'
exec 'source ' . s:plugin_config_dir . '/ultisnips.vim'
exec 'source ' . s:plugin_config_dir . '/vim-easymotion.vim'
exec 'source ' . s:plugin_config_dir . '/vim_latex_live_preview.vim'

" This one should always come last!!!
exec 'source ' . s:plugin_config_dir . '/colorscheme.vim'
" }}}

" vim:foldenable:foldmethod=marker:foldlevel=0
