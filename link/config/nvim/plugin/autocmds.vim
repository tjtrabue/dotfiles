" Autocommands are Vim/Neovim rules for automatically running certain functions
" or commands based on certain events.

" Make line numbers absolute upon entering insert mode, and
" make them relative upon leaving insert mode.
autocmd InsertEnter * :set number norelativenumber
autocmd InsertLeave * :set number relativenumber

" Enable file type detection
filetype on

" Treat .json files as .js
autocmd BufNewFile,BufRead *.json setfiletype json syntax=javascript

" Treat .md files as Markdown
autocmd BufNewFile,BufRead *.md setlocal filetype=markdown

" Strip trailing whitespace on buffer write
autocmd BufWritePre * :%s/\s+$//e

" Move cursor to last known position when reopening a file
autocmd BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$")
        \| exe "normal! g'\"" | endif

" Add syntax highlighting for additional filetypes {{{

" Interpret config files for IDE Vim plugins as Vim syntax
autocmd BufNewFile,BufRead .vrapperrc set filetype=vim syntax=vim
autocmd BufNewFile,BufRead .ideavimrc set filetype=vim syntax=vim
" }}}

" Highlight yanked text {{{
augroup highlight_yank
  autocmd!
  autocmd TextYankPost * silent!
        \lua require'vim.highlight'.on_yank{higroup="IncSearch", timeout=500}
augroup END
" }}}

" Code folding {{{

" We only want to initially fold all code in a buffer if the number of lines in
" the buffer exceeds a certain threshold. Otherwise, unfold all folds in the
" buffer.
"
" `winheight(0)` returns the height of the current window.
augroup fold_code
  autocmd BufRead * if line('$') >= winheight(0)
        \| silent execute "normal zM"
        \| else
        \| silent execute "normal zR"
        \| endif
augroup END
" }}}

" vim:foldenable:foldmethod=marker:foldlevel=0
