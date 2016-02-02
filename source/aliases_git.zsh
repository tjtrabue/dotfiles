#!/usr/bin/env zsh

# Branch alaises:
alias gch="git checkout"
alias newb="git checkout -b"
alias delb="git branch -D"
alias currb="git rev-parse --abbrev-ref HEAD"

# Logging aliases:
alias gl="git log -3"

# Info aliases:
alias gs="git status"

# Staging/pushing aliases:
alias ga="git add"
alias gc="git commit -m"
alias gca="git commit --amend"
alias gp="git push origin"

# Rebasing aliases:
alias grb="git rebase"