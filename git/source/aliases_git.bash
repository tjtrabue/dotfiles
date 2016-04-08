#!/usr/bin/env bash

# Branch alaises:
alias gch="git checkout"
alias newb="git checkout -b"
alias delb="git branch -D"
alias currb="git rev-parse --abbrev-ref HEAD"

# Logging aliases:
alias gl="git log -3"

# Info aliases:
alias gs="git status"
alias groot="git rev-parse --show-toplevel"
alias gtracked="git for-each-ref --format='%(upstream:short)' \"\$(git symbolic-ref -q HEAD)\""

# Return number of modified files:
alias gadded="git diff --cached --numstat | wc -l | sed -e \"s/[ \t]//g\""

# Staging/pushing/pulling aliases:
alias ga="git add"
alias gaa="git add \"\$(git rev-parse --show-toplevel)\"/"
alias gc="git commit -m"
alias gca="git commit --amend"
alias gcan="git commit --amend --no-edit"
alias gacan="git add \"\$(git rev-parse --show-toplevel)\"/ && git commit --amend --no-edit"
alias gp="git push"
alias gpl="git pull"

# Rebasing aliases:
alias grb="git rebase"