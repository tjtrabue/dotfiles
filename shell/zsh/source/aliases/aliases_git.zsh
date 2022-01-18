#!/usr/bin/env zsh

# Zsh handles completion with aliases much nicer than Bash.
# For instance, in Zsh you can simply write `alias gco='git checkout'`, and Zsh
# will automatically add git completions for you after you type `gco <TAB>`.
# Such is not the case with Bash. That is why we have to create all of Bash's
# git aliases programmatically in functions_completion.sh.

alias g="git"
alias ga="git add"
alias gb="git branch"
alias gbm="git blame"
alias gc="git commit"
alias gcb="git checkout -b"
alias gcl="git clone"
alias gcle="git clean"
alias gco="git checkout"
alias gcon="git config"
alias gd="git diff"
alias gdes="git describe"
alias gm="git merge"
alias gp="git push"
alias gpl="git pull"
alias grb="git rebase"
alias gre="git remote"
alias grev="git revert"
alias grm="git rm"
alias gs="git status"
alias gsb="git show-branch"
alias gsl="git shortlog"
alias gst="git stash"
alias gsw="git switch"
alias gt="git tag"

# vim:foldenable:foldmethod=marker:foldlevel=0
