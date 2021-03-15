" Neovim config file

" Redefine the user's vimrc' file to mean this file (~/.config/nvim/init.vim).
let $MYVIMRC = stdpath('config') . '/init.vim'

" Directory containing extra configuration files for plugins installed with
" vim-plug. We decide when and how to load these files. Neovim does not load
" them automatically.
let g:plugin_config_dir = stdpath('config') . '/plug-config'

" Directory containing all installed plugins
let g:plugin_install_dir = stdpath('data') . '/site/pack/packer/start'

" The file specifying all third-party plugins to use.
let g:packer_plugins_file = stdpath('config') . '/lua/plugins.lua'

" Install and activate plugins from vim-plug:
" NOTE: Currently using `packer` instead of `vim-plug`.
" exec 'source ' . stdpath('config') . '/plugins.vim'

" Load plugins defined in ./lua/plugins.lua:
lua require('plugins')

" Private functions {{{

" Source a file in the plugin-config dir.
function! s:SourcePluginConfigFile(file)

  if a:file =~ '\.vim$'
    silent execute 'source ' . g:plugin_config_dir . '/' . a:file
  elseif a:file =~ '\.lua$'
    silent execute 'luafile ' . g:plugin_config_dir . '/' . a:file
  endif
endfunction
" }}}

" Source plugin configuration files. {{{
"
" We don't want to keep these files in the standard 'plugin/' directory because
" those files are not guaranteed to load in a given order, and may even load
" asynchronously. We want to control when and where we load these plugin
" configuration files so that we don't encounter strange errors when Neovim
" first loads.

" This should be first, since which key ought to be registered before we make
" ane leader mappings.
call s:SourcePluginConfigFile('vim-which-key.vim')

call s:SourcePluginConfigFile('airline.vim')
call s:SourcePluginConfigFile('awesome_terminal_fonts.vim')
call s:SourcePluginConfigFile('ctrlp.vim')
call s:SourcePluginConfigFile('emmet.vim')
call s:SourcePluginConfigFile('gitgutter.vim')
call s:SourcePluginConfigFile('gundo.vim')
call s:SourcePluginConfigFile('incsearch-easymotion.vim')
call s:SourcePluginConfigFile('incsearch.vim')
call s:SourcePluginConfigFile('latex.vim')
call s:SourcePluginConfigFile('tagbar.vim')
call s:SourcePluginConfigFile('tmuxline.vim')
call s:SourcePluginConfigFile('ultisnips.vim')
call s:SourcePluginConfigFile('vim-easymotion.vim')
call s:SourcePluginConfigFile('vim_latex_live_preview.vim')
call s:SourcePluginConfigFile('lazygit.nvim.vim')
call s:SourcePluginConfigFile('undotree.vim')
call s:SourcePluginConfigFile('neogit.vim')
call s:SourcePluginConfigFile('git-blame.nvim.vim')
call s:SourcePluginConfigFile('glow.nvim.vim')
call s:SourcePluginConfigFile('barbar.nvim.vim')
call s:SourcePluginConfigFile('nvim-hlslens.vim')

call s:SourcePluginConfigFile('nvim-tree.vim')
" call s:SourcePluginConfigFile('chadtree.vim')

call s:SourcePluginConfigFile('indent-guides.nvim.lua')
" Currently not using fzf. Using telescope.nvim instead.
" call s:SourcePluginConfigFile('fzf.vim')
call s:SourcePluginConfigFile('telescope.nvim.lua')
call s:SourcePluginConfigFile('telescope.nvim.vim')

" Load the autocompletion engine
call s:SourcePluginConfigFile('nvim-compe.lua')

" Load the LSP configuration file.
call s:SourcePluginConfigFile('lsp.lua')

" Activate the awesome galaxyline statusbar. This status line must be entirely
" self-configured. That is, there is no 'default' status line that comes with
" galaxyline. However, the repo does contain a few example files that you can
" use as a starting point for your own custom status lines.
call s:SourcePluginConfigFile('galaxyline.vim')

" These should always come last!!!
call s:SourcePluginConfigFile('colorscheme.vim')
call s:SourcePluginConfigFile('nvim-web-devicons.lua')
" }}}

" Automatically compile new plugins whenever the plugins.lua file is
" modified.
autocmd BufWritePost g:packer_plugins_file PackerCompile

" vim:foldenable:foldmethod=marker:foldlevel=0
