#!/bin/sh

# First, make sure that `l` is not an alias, as it commonly is by default on
# many operating systems.
alias l >>/dev/null 2>&1 && unalias l >>/dev/null 2>&1
# List out files in long-form, using `exa` if available, or `ls`.
l() {
  local flags="-lFh"
  local dir_to_list="${*:-.}"

  __do_ls "${flags}" "${dir_to_list}" "light"
}

alias ll >>/dev/null 2>&1 && unalias ll >>/dev/null 2>&1
# List out files in long-form, choosing the `ls` analog command automatically.
ll() {
  local flags="-lFh"
  local dir_to_list="${*:-.}"
  __do_ls "$flags" "$dir_to_list"
}

alias la >>/dev/null 2>&1 && unalias la >>/dev/null 2>&1
# List all files in long-form, using either `exa` or `ls`.
la() {
  local flags="-lFhA"
  local dir_to_list="${*:-.}"
  __do_ls "${flags}" "${dir_to_list}" "light"
}

alias lla >>/dev/null 2>&1 && unalias lla >>/dev/null 2>&1
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
  local lightweight="${3}"

  flags="${flags} $(__get_ls_colorflag)"
  if [ -n "${lightweight}" ]; then
    __do_ls_light "${flags}" "${dir_to_list}"
  else
    __do_ls_auto "${flags}" "${dir_to_list}"
  fi
}

# Run a lightweight `ls`-compatible command, which mitigates the latency of
# commands such as `colorls` or `ls-icons`.
__do_ls_light() {
  local flags="$1"
  local dir_to_list="${2:-.}"

  if [ -x "$(command -v "exa")" ]; then
    __do_ls_exa "${flags}" "${dir_to_list}"
  elif [ -x "$(command -v perl)" ]; then
    __do_ls_perl_script "${flags}" "${dir_to_list}"
  else
    __do_ls_standard "${flags}" "${dir_to_list}"
  fi
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
    __do_ls_standard "${flags}" "${dir_to_list}"
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

  # `exa` has no '-A' flag, so use '-a' instead.
  flags="${flags/A/a}"
  # Add extra exa-specific flags.
  flags="${flags} -@ --group-directories-first --git"
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
    colorflag="--color=always"
  else # macOS `ls`
    colorflag="-G"
  fi

  echo "${colorflag}"
}

# Installs a custom ls colors database from the LS_COLORS Git repository.
install_custom_ls_colors() {
  local installDir="/tmp/LS_COLORS"
  local lscolorsScript="${HOME}/.local/share/lscolors.sh"
  local tarballUrl="https://api.github.com/repos/trapd00r/LS_COLORS/tarball/master"

  log_info "Installing custom LS_COLORS theme at:" \
    "${GREEN}${lscolorsScript}${NC}"

  mkdir -p "${installDir}"
  curl -L "${tarballUrl}" | tar xzf - --directory="${installDir}" --strip=1
  (cd "${installDir}" && make install)
}

# Export LS_COLORS with the appropriate color settings for the current shell.
src_dircolors_for_profile() {
  local lscolorsScript="${HOME}/.local/share/lscolors.sh"

  if [ ! -f "${lscolorsScript}" ]; then
    install_custom_ls_colors
  fi

  log_info "Activating custom LS_COLORS for 'ls' output"
  . "${lscolorsScript}"
}

# vim:foldenable:foldmethod=indent::foldnestmax=1
