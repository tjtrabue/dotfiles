
" A sample galaxyline status file written in lua that comes with the galaxyline
" repository.
let s:example_galaxyline_file = g:vim_plugged_dir .
      \ '/galaxyline.nvim/example/eviline.lua'

" Figure out how to activate the galaxyline statusbar.
if filereadable(s:example_galaxyline_file)
  " Pull in example statusline included in galaxyline repository.
  execute 'luafile ' . s:example_galaxyline_file
else
  " If we can't find the example ilfe, default to the one we copied/configured.
  execute 'luafile ' . g:plugin_config_dir . '/galaxyline.lua'
endif

" vim:foldenable:foldmethod=marker:foldlevel=0
