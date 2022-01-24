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
" any leader mappings.
call s:SourcePluginConfigFile('vim-which-key.config.vim')

call s:SourcePluginConfigFile('airline.config.vim')
call s:SourcePluginConfigFile('awesome_terminal_fonts.config.vim')
call s:SourcePluginConfigFile('ctrlp.config.vim')
call s:SourcePluginConfigFile('emmet.config.vim')
call s:SourcePluginConfigFile('vim-signify.config.vim')
call s:SourcePluginConfigFile('gundo.config.vim')
call s:SourcePluginConfigFile('incsearch-easymotion.config.vim')
call s:SourcePluginConfigFile('incsearch.config.vim')
call s:SourcePluginConfigFile('latex.config.vim')
call s:SourcePluginConfigFile('tagbar.config.vim')
call s:SourcePluginConfigFile('tmuxline.config.vim')
call s:SourcePluginConfigFile('ultisnips.config.vim')
call s:SourcePluginConfigFile('vim-easymotion.config.vim')
call s:SourcePluginConfigFile('vim_latex_live_preview.config.vim')
call s:SourcePluginConfigFile('lazygit.config.vim')
call s:SourcePluginConfigFile('undotree.config.vim')
call s:SourcePluginConfigFile('neogit.lua')
call s:SourcePluginConfigFile('neogit.config.vim')
call s:SourcePluginConfigFile('git-blame.config.vim')
call s:SourcePluginConfigFile('glow.config.vim')
call s:SourcePluginConfigFile('barbar.config.vim')
call s:SourcePluginConfigFile('nvim-hlslens.config.vim')
call s:SourcePluginConfigFile('formatter.nvim.lua')
call s:SourcePluginConfigFile('ranger.config.vim')

call s:SourcePluginConfigFile('nvim-tree.config.vim')
" call s:SourcePluginConfigFile('chadtree.config.vim')

call s:SourcePluginConfigFile('indent-guides.nvim.lua')
" Currently not using fzf. Using telescope.nvim instead.
" call s:SourcePluginConfigFile('fzf.config.vim')
call s:SourcePluginConfigFile('telescope.nvim.lua')
call s:SourcePluginConfigFile('telescope.config.vim')

" Load auto-completion configuration
call s:SourcePluginConfigFile('nvim-cmp.lua')

" Load Aerial code browser config file
call s:SourcePluginConfigFile('aerial.nvim.lua')

" Load the LSP configuration file.
call s:SourcePluginConfigFile('lsp.lua')

" Better defaults for nvim-lsp.
call s:SourcePluginConfigFile('nvim-lsputils.lua')

" Trouble is a pretty LSP diagnostic browser for Neovim.
call s:SourcePluginConfigFile('trouble.nvim.lua')

" Define new textobjects with the help of treesitter and LSP.
call s:SourcePluginConfigFile('treesitter-textobjs.lua')

" Define treesitter configuration
call s:SourcePluginConfigFile('nvim-treesitter.conf.lua')

" Highlight colors in files for color names, RGB values, and hex codes.
call s:SourcePluginConfigFile('nvim-colorizer.lua')

" These should always come last!!!
call s:SourcePluginConfigFile('colorscheme.config.vim')
call s:SourcePluginConfigFile('nvim-web-devicons.lua')

" Activate the awesome galaxyline statusbar. This status line must be entirely
" self-configured. That is, there is no 'default' status line that comes with
" galaxyline. However, the repo does contain a few example files that you can
" use as a starting point for your own custom status lines.
call s:SourcePluginConfigFile('galaxyline.config.vim')

" lualine is a fast, minimal framework for building statuslines.
" call s:SourcePluginConfigFile('lualine.nvim.lua')

" Org mode configuration
call s:SourcePluginConfigFile('orgmode.nvim.lua')

" Code linter that works alongside Neovim's LSP.
call s:SourcePluginConfigFile('nvim-lint.lua')

" PostgreSQL syntax highlighting config
call s:SourcePluginConfigFile('pgsql.config.vim')
" }}}

" Automatically compile new plugins whenever the plugins.lua file is
" modified.
autocmd BufWritePost g:packer_plugins_file PackerCompile

" vim:foldenable:foldmethod=marker:foldlevel=0
