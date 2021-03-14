" Neovim config file

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

" This one should always come last!!!
exec 'source ' . s:plugin_config_dir . '/colorscheme.vim'
" }}}

" vim:foldenable:foldmethod=marker:foldlevel=0
