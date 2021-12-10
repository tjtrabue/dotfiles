#!/bin/sh

# Inject extra environment variables into the ~/.var_overrides file, if prudent.
add_extra_os_vars() {
  local userVars="${VAR_OVERRIDES_FILE:-${HOME}/.var_overrides}"
  local os="$(getdistro)"
  local dotCopy="${DOTFILES_COPY:-${DOTFILES_HOME}/copy}"
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

  if [ ! -s "${extraVarsFile}" ]; then
    # Create the ~/.var_overrides file if necessary with basic information.
    command cat <<EOF >>"${userVars}"
#!/bin/sh

# This file holds machine-local environment variables that should not be tracked
# in ~/.vars.
EOF
  else
    # Make a backup of the ~/.var_overrides file, just in case.
    cp "${userVars}"{,.bak}
  fi

  # Get rid of the Vim modeline (will re-add it after)
  sed -i '/# vim:foldenable.*/d' "${userVars}"
  # Inject variables
  command cat "${extraVarsFile}" >>"${userVars}"
  # Re-add the modeline for Vim
  printf '\n%s' '# vim:foldenable:foldmethod=marker:foldlevel=0' >>"${userVars}"

  log_info "Done injecting additional OS variables"
}
