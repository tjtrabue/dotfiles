#!/bin/sh

# First, make sure that `l` is not an alias, as it commonly is by default on
# many operating systems.
unalias l >>/dev/null 2>&1
# List out files in long-form, using `exa` if available, or `ls`.
l() {
  local flags="-lFh"
  local dir_to_list="${*:-.}"

  if [ -x "$(command -v exa)" ]; then
    __do_ls_exa "${flags}" "${dir_to_list}"
  else
    __do_ls_standard "${flags}" "${dir_to_list}"
  fi
}

unalias ll >>/dev/null 2>&1
# List out files in long-form, choosing the `ls` analog command automatically.
ll() {
  local flags="-lFh"
  local dir_to_list="${*:-.}"
  __do_ls "$flags" "$dir_to_list"
}

unalias la >>/dev/null 2>&1
# List all files in long-form, using either `exa` or `ls`.
la() {
  local flags="-lFh"
  local dir_to_list="${*:-.}"

  if [ -x "$(command -v exa)" ]; then
    flags="${flags}a"
    __do_ls_exa "${flags}" "${dir_to_list}"
  else
    flags="${flags}A"
    __do_ls_standard "${flags}" "${dir_to_list}"
  fi
}

unalias lla >>/dev/null 2>&1
# List all files in long-form, choosing the `ls` analog command automatically.
lla() {
  local flags="-lAFh"
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

  if [ -x "$(command -v colorls)" ]; then
    __do_ls_colorls "${flags}" "${dir_to_list}"
  elif $USE_LS_ICONS && [ -x "$(command -v "ls-icons")" ]; then
    __do_ls_ls_icons "${flags}" "${dir_to_list}"
  elif [ -x "$(command -v "exa")" ]; then
    __do_ls_exa "${flags}" "${dir_to_list}"
  elif [ -x "$(command -v perl)" ]; then
    __do_ls_perl_script "${flags}" "${dir_to_list}"
  else
    # If all else fails, just run ls
    eval "command ls ${flags} ${dir_to_list}"
  fi
}

# Ruby's amazing colorls tool is probably the best ls colorizer out there.
# Take out flags that colorls does not understand.
__do_ls_colorls() {
  local flags="${1}"
  local dir_to_list="${2:-.}"

  flags="$(echo "$flags" | sed -r 's/[Fh]//')"
  flags="${flags} --sd --gs"
  eval "command colorls ${flags} ${dir_to_list}"
}

# The exa command is a drop-in replacement for ls written in rust, and has
# far better coloring capabilities.
__do_ls_exa() {
  local flags="$1"
  local dir_to_list="${2:-.}"
  eval "command exa ${flags} ${dir_to_list}"
}

# Use the fancy ls-icons tool to show ls output if we have ls-icons installed.
__do_ls_ls_icons() {
  local flags="$1"
  local dir_to_list="${2:-.}"
  eval "command ls-icons ${flags} ${dir_to_list}"
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
    colorflag="--color=auto"
  else # OS X `ls`
    colorflag="-G"
  fi

  echo "${colorflag}"
}

# vim:foldenable:foldmethod=indent::foldnestmax=1
