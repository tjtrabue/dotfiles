#!/usr/bin/env zsh

# Checkout aliases:
alias gco="git checkout"
alias gcob="git checkout -b";
alias newb="git checkout -b"

# Branch aliases:
alias gb="git branch";
alias gbd="git branch -d";
alias gbD="git branch -D";

# Logging aliases:
alias gl="git log -3";

# Info aliases:
alias gs="git status";
alias groot="git rev-parse --show-toplevel"

# Return number of modified files:
alias gadded="git diff --cached --numstat | wc -l | sed -e \"s/[ \t]//g\""

# Staging/pushing aliases:
alias ga="git add"
alias gaa="git add --all";
alias gc="git commit";
alias gca="git commit --amend";
alias gcm="git commit -m";

# Pushing aliases:
alias gp="git push";
alias gpo="git push origin";
alias gpoh="git push origin HEAD";

# Rebasing aliases:
alias grb="git rebase"