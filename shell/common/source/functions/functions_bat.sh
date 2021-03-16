#!/bin/sh

# If the `bat` tool is installed, add the following functions:
if [ -x "$(command -v bat)" ]; then
  # Combine `bat` with git-diff to view lines around code changes with proper
  # syntax highlighting:
  bdiff() {
    git diff --name-only --diff-filter=d | xargs bat --diff
  }

  # Combine `bat` with git-show to display an older version of a file with
  # syntax highlighting.
  bshow() {
    git show "${@}" | bat -l rs
  }

  # Tail a given file using bat's syntax highlighting capabilities
  btf() {
    local logFile="${*}"

    tail -f "${logFile}" | bat --paging=never -l log
  }
fi

# Install the bat-extras tools, which allow bat to integrate nicely with
# existing tools.
install_bat_extras() {
  local batExtrasUrl="https://github.com/eth-p/bat-extras.git"
  local batExtrasDest="${WS:-${HOME}/workspace}"
  local batExtrasDir="${batExtrasDest}/bat-extras"
  local batExtrasBinDest="${HOME}/.local/bin"
  local script

  mkdir -p "${batExtrasDest}" "${batExtrasBinDest}"

  if [ ! -d "${batExtrasDir}" ]; then
    log_info "Cloning bat extras..."
    git clone "${batExtrasUrl}" "${batExtrasDir}"
  else
    git -C "${batExtrasDir}" pull
  fi

  log_info "Installing bat-extras scripts to: ${batExtrasBinDest}/"
  for script in "${batExtrasDir}/src"/*.sh; do
    # Remove the ".sh" extension from the bat scripts as we install them.
    install -m 755 -T "${script}" \
      "${batExtrasBinDest}"/$(basename -s '.sh' "${script}")
  done
}

# vim:foldenable:foldmethod=indent:foldlevel=1:foldnestmax=1
