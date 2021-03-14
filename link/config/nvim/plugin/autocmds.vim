" Autocommands are Vim/Neovim rules for automatically running certain functions
" or commands based on certain events.

" EVERY auto command in this section should be inside of the enclosing 'if'
" statement to avoid compatibility issues with versions of Vim that do not have
" autocmd functions.
if has('autocmd')
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

    " Interpret config files for IDE Vim plugins as Vim syntax
    autocmd BufNewFile,BufRead .vrapperrc set filetype=vim syntax=vim
endif
