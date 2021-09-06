" Whether to use my own custom galaxyline theme or default to the example theme
" included with the galaxyline repository.
" 1 == true
" 0 == false
let s:use_custom_theme = 1

" A sample galaxyline status file written in lua that comes with the galaxyline
" repository.
let s:example_galaxyline_file = g:plugin_install_dir .
      \ '/galaxyline.nvim/example/eviline.lua'

" Figure out how to activate the galaxyline statusbar.
if filereadable(s:example_galaxyline_file) && ! s:use_custom_theme
  " Pull in example statusline included in galaxyline repository.
  execute 'luafile ' . s:example_galaxyline_file
else
  " If we can't find the example ilfe, default to the one we copied/configured.
  execute 'luafile ' . g:plugin_config_dir . '/galaxyline.lua'
endif

" vim:foldenable:foldmethod=marker:foldlevel=0
