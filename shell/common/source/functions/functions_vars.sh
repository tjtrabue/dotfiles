#!/bin/sh

# Inject extra environment variables into the ~/.var_overrides file, if prudent.
add_extra_os_vars() {
  local userVars="${VAR_OVERRIDES_FILE:-${HOME}/.var_overrides}"
  local os="$(getdistro)"
  local dotHome="${DOTFILES_HOME:-${HOME}/.dotfiles}"
  local dotCopy="${DOTFILES_COPY:-${dotHome}/copy}"
  local extraVarsDir="${dotCopy}/var_files"
  local extraVarsLinuxDir="${extraVarsDir}/linux"
  local extraVarsMacDir="${extraVarsDir}/mac"
  local extraVarsFile="NONE"

  case "${os}" in
  "Arch Linux")
    log_info "Injecting Arch Linux vars into: ${BLUE}${userVars}${NC}"
    extraVarsFile="${extraVarsLinuxDir}/arch_vars.bash"
    ;;
  "Darwin")
    log_info "Injecting macOS vars into: ${BLUE}${userVars}${NC}"
    extraVarsFile="${extraVarsMacDir}/mac_vars.bash"
    ;;
  *)
    warn "No extra vars to add for OS: ${RED}${os}${NC}"
    return 0
    ;;
  esac

  # Backup the var_overrides file before proceeding.
  if [ -f "${userVars}" ]; then
    cp -f "${userVars}"{,.bak}
  else
    __init_var_overrides_file "${userVars}"
  fi

  if [ -f "${extraVarsFile}" ]; then
    __inject_var_file_contents_into_var_overrides "${extraVarsFile}"
  fi

  log_info "Done injecting additional OS variables"
}

# Injects the contents of a given file into ~/.var_overrides, if
# ~/.var_overrides does not already contain the contents of that file.
__inject_var_file_contents_into_var_overrides() {
  local extraVarsFile="${1}"
  local userVars="${VAR_OVERRIDES_FILE:-${HOME}/.var_overrides}"

  if ! filecontentsinfile "${extraVarsFile}" "${userVars}"; then
    log_info "Injecting vars from ${BLUE}${extraVarsFile}${NC} into" \
      "${BLUE}${userVars}${NC}"
    # Get rid of the Vim modeline (will re-add it after)
    sed -i '/# vim:foldenable.*/d' "${userVars}"
    # Inject variables
    command cat "${extraVarsFile}" >>"${userVars}"
    # Re-add the modeline for Vim
    printf '\n%s' '# vim:foldenable:foldmethod=marker:foldlevel=0' >>"${userVars}"
  else
    log_info "All vars from ${BLUE}${extraVarsFile}${NC} already present in" \
      "${BLUE}${userVars}${NC}"
  fi
}

__init_var_overrides_file() {
  local userVars="${1:-${HOME}/.var_overrides}"

  log_info "Initializing empty var overrides file: ${BLUE}${userVars}${NC}"

  command cat <<EOF >>"${userVars}"
#!/bin/sh

# This file holds machine-local environment variables that should not be tracked
# in ~/.vars.
EOF
}
