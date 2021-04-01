#!/bin/sh

# Re-copy the .vars file from the dotfiles repository to the user's home
# directory.
sync_vars() {
  local dotCopy="${DOTFILES_COPY:-${DOTFILES_HOME}/copy}"
  local dotVars="${dotCopy}/.vars"
  local userVars="${HOME}/.vars"

  if [ ! -f "${dotVars}" ]; then
    err "No ${GREEN}.vars${NC} file found in: ${BLUE}${dotCopy}${NC}"
    return 1
  fi

  __backup_user_vars_file

  log_info "Copying ${GREEN}${dotVars}${NC} to ${GREEN}${userVars}${NC}"
  cp -f "${dotVars}" "${userVars}"

  __add_extra_os_vars
}

# Make a backup of the ~/.vars file before re-copying the dotfiles .vars file
# to ~/.vars
__backup_user_vars_file() {
  local userVars="${HOME}/.vars"

  if [ -f "${userVars}" ]; then
    log_info "Backing up user's ${userVars} file"
    mv -f "${userVars}"{,.bak}
  fi
}

# Inject extra environment variables into the ~/.vars file, if prudent.
__add_extra_os_vars() {
  local os="$(getosinfo | head -1 | sed 's/Distribution:\s*//')"
  local dotCopy="${DOTFILES_COPY:-${DOTFILES_HOME}/copy}"
  local extraVarsLinuxDir="${dotCopy}/var_files/linux"
  local userVars="${HOME}/.vars"
  local markerString="#<additional-vars-insert>"
  local extraVarsFile="NONE"

  log_info "Injecting additional OS variables into ${userVars}"

  case "${os}" in
    "Arch Linux")
      log_info "Injecting Arch Linux vars into ${userVars}"
      extraVarsFile="${extraVarsLinuxDir}/arch_vars.bash"
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
  log_info "Done injecting additional variables"
}
