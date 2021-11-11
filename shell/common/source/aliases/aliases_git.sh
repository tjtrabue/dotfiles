#!/bin/sh

### NOTE CONCERNING "MISSING" ALIASES:
# Many of the most common git aliases are listed in functions_completion.bash,
# which contains not only aliases, but completion directives that allow those
# git command aliases to use bash-completion's awesome git completions. I
# decided to list them in functions_completions.bash for cohesion, meaning that
# I only have to look in one place if I want to change both the alias and its
# completion function.

# Branch alaises {{{
alias newb="git checkout -b"
alias delb="git branch -D"
alias currb="git rev-parse --abbrev-ref HEAD"
# }}}

# Info aliases {{{
alias groot="git rev-parse --show-toplevel"
alias gtracked="git for-each-ref --format='%(upstream:short)' \"\$(git symbolic-ref -q HEAD)\""
# }}}

# Diff aliases {{{
alias gdc="git diff --cached"
# Return 0 if no tracked files have been changed.
alias nochanges="git diff --exit-code --quiet"
# Return 0 if no changed files have been added to the index.
alias noadded="git diff --cached --exit-code --quiet"
# }}}

# Difftool {{{
alias gdt="git difftool --dir-diff"
# }}}

# Return number of modified files {{{
alias gadded="git diff --cached --numstat | wc -l | sed -e \"s/[ \t]//g\""
# }}}

# Staging/pushing/pulling aliases {{{
alias gaa="git add -A"
alias gca="git commit --amend"
alias gcan="git commit --amend --no-edit"
alias gacan="git add \"\$(git rev-parse --show-toplevel)\"/ && git commit --amend --no-edit"
# }}}

# Commit log aliases {{{
alias gl='git log --graph --color=always --format="%C(auto)%h%d %s %C(blue)%C(bold)%cr"'
alias gll='git log --graph --color=always --format=full'
# }}}

# Mergetool {{{
alias gmt="git mergetool"
# }}}

# Rebasing aliases {{{
alias grba="git rebase --abort"
alias grbc="git rebase --continue"
# }}}

# Resetting aliases {{{
alias greh="git reset HEAD"
# }}}

# lazygit {{{
# Activate lazygit porcelain tool.
alias lg="lazygit"
# }}}

# vim:foldenable:foldmethod=marker:foldlevel=0
