#!/bin/sh

# This file performs common initialization tasks for POSIX shells.
# This file should be sourced as soon as possible by any POSIX-compliant
# initialization file for the shell (i.e, by .bashrc or .zshrc) so that
# common functions and aliases can be brought into the shell's environment.

# Source all functions and alias files for any POSIX-compliant shell.
src() {
  local currentShell="$(ps -p $$ | awk '{print $NF}' | tail -1)"
  local dotfilesHome="${DOTFILES_HOME:-${HOME}/.dotfiles}"
  local dotfilesShell="${dotfilesHome}/shell"
  local dotfilesBash="${dotfilesShell}/bash"
  local dotfilesZsh="${dotfilesShell}/zsh"
  local dotfilesCommonSource="${dotfilesShell}/common/source"
  local srcDir
  local f
  local d

  # Determine in which directory our shell-specific aliases and functions
  # reside.
  if [ "${currentShell}" = "bash" ]; then
    srcDir="${dotfilesBash}/source"
  elif [ "${currentShell}" = "zsh" ]; then
    srcDir="${dotfilesZsh}/source"
  fi

  # Source environment variables and directory aliases.
  for f in "${HOME}"/.{vars,dirs}; do
    [ -f "${f}" ] && . "${f}"
  done

  # Main alias/function sourcing logic
  for d in {aliases,functions}; do
    if [ -d "${dotfilesCommonSource}/${d}" ]; then
      # Load common aliases and functions.
      for f in "${dotfilesCommonSource}/${d}"/*; do
        . "${f}"
      done
    fi
    if [ -d "${srcDir}/${d}" ]; then
      # Load shell-specific aliases and functions.
      for f in "${srcDir}/${d}"/*; do
        . "${f}"
      done
    fi
  done
}

src

# Add extra binary paths to $PATH
spath

# vim:foldenable:foldmethod=marker:foldlevel=0
