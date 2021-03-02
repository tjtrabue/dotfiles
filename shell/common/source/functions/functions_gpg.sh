#!/bin/sh

# Communicate with a running gpg-agent, or start a new instance if none are
# currently running.
gpgagent() {
  local gpgAgentFile="${HOME}/.gpg-agent-info"

  if [ -f "${gpgAgentFile}" ]; then
    __load_existing_gpg_agent_from_file

    if ! __check_gpg_agent_running; then
      # If the gpg-agent file's information is stale, we will need to start a
      # new gpg-agent and recreate the gpg-agent information file.
      log_info "Killing any lingering gpg-agent processes..."
      pkill "gpg-agent"
      log_info "Removing old ${gpgAgentFile} file"
      rm -f "${gpgAgentFile}"
      __create_gpg_agent
    fi
  else
    __create_gpg_agent
  fi

  __set_current_tty_for_gpg
}

__check_gpg_agent_running() {
  pgrep gpg-agent >>/dev/null 2>&1
}

__load_existing_gpg_agent_from_file() {
  local gpgAgentFile="${HOME}/.gpg-agent-info"

  log_info "Reading GPG agent information from file: ${gpgAgentFile}"
  . "${gpgAgentFile}"
  export GPG_AGENT_INFO
  export SSH_AUTH_SOCK
}

# Construct a new instance of gpg-agent and write its environment properties to
# a file for future reference. This avoids creating unnecessary duplicate
# gpg-agents in new terminals.
__create_gpg_agent() {
  local gpgAgentFile="${HOME}/.gpg-agent-info"

  log_info "Creating new GPG agent"
  gpg-agent --daemon --enable-ssh-support \
    --write-env-file "${gpgAgentFile}" >>/dev/null 2>&1
}

# Should always export GPG_TTY! This sets the current terminal as the one to
# receive GPG information.
__set_current_tty_for_gpg() {
  log_info "Setting current TTY for GPG input/output."
  GPG_TTY="$(tty)"
  export GPG_TTY
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
