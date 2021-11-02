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

" Interpret the ~/.extra file as shell syntax
autocmd BufNewFile,BufRead .extra set filetype=sh syntax=sh

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

" vim:foldenable:foldmethod=marker:foldlevel=0
