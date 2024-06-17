#!/bin/sh

# First, make sure that `l` is not an alias, as it commonly is by default on
# many operating systems.
alias l >>/dev/null 2>&1 && unalias l >>/dev/null 2>&1
# List out files in long-form, using `exa` if available, or `ls`.
l() {
  local flags="-l -F -h"
  local dir_to_list="${*:-.}"

  __do_ls "${flags}" "${dir_to_list}"
}

alias ll >>/dev/null 2>&1 && unalias ll >>/dev/null 2>&1
# List out files in long-form, choosing the `ls` analog command automatically.
ll() {
  local flags="-l -F -h"
  local dir_to_list="${*:-.}"
  __do_ls "$flags" "$dir_to_list"
}

alias la >>/dev/null 2>&1 && unalias la >>/dev/null 2>&1
# List all files in long-form, using either `eza`, `exa`, or `ls`.
la() {
  local flags="-l -F -h -A"
  local dir_to_list="${*:-.}"
  __do_ls "${flags}" "${dir_to_list}"
}

alias lla >>/dev/null 2>&1 && unalias lla >>/dev/null 2>&1
# List all files in long-form, choosing the `ls` analog command automatically.
lla() {
  local flags="-l -A -F -h"
  local dir_to_list="${*:-.}"
  __do_ls "$flags" "$dir_to_list"
}

# Figure out which version of 'ls' we want to run based on what's installed, and
# then run the darn thing.
__do_ls() {
  local flags="$1"
  local dir_to_list="${2:-.}"

  flags="${flags} $(__get_ls_colorflag)"
  __do_ls_auto "${flags}" "${dir_to_list}"
}

# Figure out which ls command to run automatically based on a prioritized list.
__do_ls_auto() {
  local flags="$1"
  local dir_to_list="${2:-.}"

  if [ -x "$(command -v "eza")" ]; then
    __do_ls_eza "${flags}" "${dir_to_list}"
  elif [ -x "$(command -v "exa")" ]; then
    __do_ls_exa "${flags}" "${dir_to_list}"
  elif [ -x "$(command -v perl)" ]; then
    __do_ls_perl_script "${flags}" "${dir_to_list}"
  else
    __do_ls_standard "${flags}" "${dir_to_list}"
  fi
}

# The exa command is a drop-in replacement for ls written in rust, and has
# far better coloring capabilities.
#
# NOTE: `exa` has been replaced by the `eza` tool.
__do_ls_exa() {
  local flags="$1"
  local dir_to_list="${2:-.}"

  # `exa` has no '-A' flag, so use '-a' instead.
  flags="${flags/A/a}"
  # Add extra exa-specific flags.
  flags="${flags} -@ --group-directories-first --git"
  eval "command exa ${flags} ${dir_to_list}"
}

# `eza` is the successor program to exa, and should be used in its stead.
__do_ls_eza() {
  local flags="$1"
  local dir_to_list="${2:-.}"

  # `eza` has no '-A' flag, so use '-a' instead.
  flags="${flags/A/a}"
  # Add extra eza-specific flags.
  flags="${flags} -@ --group-directories-first --git"
  eval "command eza ${flags} ${dir_to_list}"
}

# Use our beautiful perl coloration script if we have access to perl
__do_ls_perl_script() {
  local flags="$1"
  local dir_to_list="${2:-.}"
  local ls_color_script="${DOTFILES_HOME}/perl/ls-color.pl"
  eval "command ls ${flags} ${dir_to_list} | perl ${ls_color_script}"
}

# The regular, plain old `ls` command. No frills, no modifications.
__do_ls_standard() {
  local flags="$1"
  local dir_to_list="${2:-.}"
  eval "command \ls ${flags} ${dir_to_list}"
}

# Retrieve the output coloration flag for ls based on the installed ls version.
__get_ls_colorflag() {
  local colorflag

  # Detect which `ls` flavor is in use
  if ls --color=auto >/dev/null 2>&1; then # GNU `ls`
    colorflag="--color=always"
  else # macOS `ls`
    colorflag="-G"
  fi

  echo "${colorflag}"
}

# Installs a custom ls colors database from the LS_COLORS Git repository.
install_custom_ls_colors() {
  local installDir="${HOME}/.ls_colors"
  local lscolorsScript="${installDir}/lscolors.sh"
  local repoUrl="https://github.com/trapd00r/LS_COLORS.git"

  if [ -d "${installDir}" ]; then
    git -C "${installDir}" clean -df
    git -C "${installDir}" restore .
    git -C "${installDir}" pull
  else
    log_info "Installing custom LS_COLORS theme at:" \
      "${GREEN}${installDir}${NC}"
    git clone "${repoUrl}" "${installDir}"
  fi

  (
    cd "${installDir}"
    make install
  )
}

# Export LS_COLORS with the appropriate color settings for the current shell.
src_dircolors_for_profile() {
  local lscolorsScript="${HOME}/.ls_colors/lscolors.sh"

  if [ ! -f "${lscolorsScript}" ]; then
    install_custom_ls_colors
  fi

  log_info "Activating custom LS_COLORS for 'ls' output"
  . "${lscolorsScript}"
}

# vim:foldenable:foldmethod=indent::foldnestmax=1
