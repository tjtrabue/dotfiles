#!/usr/bin/env bash

alias g="git"

# Branch alaises:
alias gch="git checkout"
alias newb="git checkout -b"
alias delb="git branch -D"
alias currb="git rev-parse --abbrev-ref HEAD"

# Info aliases:
alias gs="git status"
alias groot="git rev-parse --show-toplevel"
alias gtracked="git for-each-ref --format='%(upstream:short)' \"\$(git symbolic-ref -q HEAD)\""

# Diff aliases
alias gd="git diff"
alias gdc="git diff --cached"

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
alias gpoh="git push origin HEAD"
alias gpl="git pull"

# Commit log aliases
alias gl='git log --graph --color=always --format="%C(auto)%h%d %s %C(blue)%C(bold)%cr"'
alias gll='git log --graph --color=always --format=full'

# Rebasing aliases:
alias grb="git rebase"
alias grba="git rebase --abort"
alias grbc="git rebase --continue"

# Resetting aliases:
alias greh="git reset HEAD"
