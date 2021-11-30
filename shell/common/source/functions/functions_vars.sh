#!/bin/sh

# Re-copy the .vars file from the dotfiles repository to the user's home
# directory. Handy when very few local changes to ~/.vars have been made, and
# you want to gain access to new additions to the .vars file in the dotfiles
# repo.
#
# This function makes a backup of the user's existing ~/.vars file
# before copying the dotfiles .vars file so that the user may restore their
# settings if need be.
varsync() {
  local dotCopy="${DOTFILES_COPY:-${DOTFILES_HOME}/copy}"
  local dotVars="${dotCopy}/dotfiles_to_copy/.vars"
  local userVars="${HOME}/.vars"

  if [ ! -f "${dotVars}" ]; then
    err "No ${GREEN}.vars${NC} file found in: ${BLUE}${dotCopy}${NC}"
    return 1
  fi

  __backup_user_vars_file "${userVars}"

  log_info "Copying ${GREEN}${dotVars}${NC} to ${GREEN}${userVars}${NC}"
  cp -f "${dotVars}" "${userVars}"

  __add_extra_os_vars "${userVars}"
}

# Make a backup of the ~/.vars file before re-copying the dotfiles .vars file
# to ~/.vars
__backup_user_vars_file() {
  local userVars="${1:-${HOME}/.vars}"

  if [ -f "${userVars}" ]; then
    log_info "Backing up user's ${userVars} file"
    mv -f "${userVars}"{,.bak}
  fi
}

# Inject extra environment variables into the ~/.vars file, if prudent.
__add_extra_os_vars() {
  local userVars="${1:-${HOME}/.vars}"
  local os="$(getdistro)"
  local dotCopy="${DOTFILES_COPY:-${DOTFILES_HOME}/copy}"
  local extraVarsDir="${dotCopy}/var_files"
  local extraVarsLinuxDir="${extraVarsDir}/linux"
  local extraVarsMacDir="${extraVarsDir}/mac"
  local markerString="#<additional-vars-insert>"
  local extraVarsFile="NONE"

  log_info "Injecting additional OS variables into ${userVars}"

  case "${os}" in
  "Arch Linux")
    log_info "Injecting Arch Linux vars into ${userVars}"
    extraVarsFile="${extraVarsLinuxDir}/arch_vars.bash"
    ;;
  "Darwin")
    log_info "Injecting macOS vars into ${userVars}"
    extraVarsFile="${extraVarsMacDir}/mac_vars.bash"
    ;;
  *)
    log_info "No extra vars to add for OS: ${os}"
    ;;
  esac

  if [ -f "${extraVarsFile}" ]; then
    sed -i -e "/${markerString}/r ${extraVarsFile}" "${userVars}"
  fi

  # Get rid of marker string in ~/.vars
  sed -i "/${markerString}/d" "${userVars}"
  log_info "Done injecting additional OS variables"
}
