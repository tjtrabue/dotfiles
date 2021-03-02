#!/bin/sh

sshagent() {
  # NOTE: We do not want to manipulate running agents if we're connected to a
  #       remote host over SSH! Instead, we should take advantage of agent
  #       forwarding in such scenarios.
  if [ -n "${SSH_TTY}" ]; then
    warn "Shell running in SSH terminal. Not starting an agent."
    return 0
  fi

  __check_ssh_agent_loaded
  if [ "$?" = 2 ]; then
    # Attempt to load the running agent's configuration from its file.
    __load_ssh_agent_from_file

    __check_ssh_agent_loaded
    if [ "$?" = 2 ]; then
      log_info "Could not communicate with agent."
      log_info "Killing all running SSH agents..."
      pkill "ssh-agent"
      log_info "Starting new SSH agent..."
      __create_new_ssh_agent
      # Load the running agent into the current shell.
      __load_ssh_agent_from_file
    fi
  fi

  if __check_ssh_agent_running; then
    log_info "SSH agent running with PID: ${SSH_AGENT_PID}"
    __add_ssh_identities
  else
    __print_error_message_could_not_start_ssh_agent
    return 1
  fi
}

# Attempt to load a running SSH agent into the current shell from its
# environment file.
__load_ssh_agent_from_file() {
  # The file containing the SSH agent's PID and environment variables.
  local sshAgentFile="${SSH_AGENT_FILE:-${HOME}/.ssh-agent}"
  if [ -r "${sshAgentFile}" ]; then
    log_info "Attempting to communicate with agent from file ${sshAgentFile}..."
    eval "$(<"${sshAgentFile}")" >/dev/null
  else
    log_info "No agent file located at ${sshAgentFile}"
  fi
}

# Start a new SSH agent and write its information to the file system.
__create_new_ssh_agent() {
  local sshAgentFile="${SSH_AGENT_FILE:-${HOME}/.ssh-agent}"
  log_info "Starting new SSH agent..."
  (umask 066; ssh-agent >"${sshAgentFile}")
}

# Test if an SSH agent is registered in the current shell and running.
__check_ssh_agent_loaded() {
  ssh-add -l >>/dev/null 2>&1
}

# Return 0 if an SSH agent process is found on the system. This function does
# not check whether or not an agent is loaded into the current shell, only
# whether an agent process exists.
__check_ssh_agent_running() {
  [ -n "${SSH_AGENT_PID}" ] && ps -p "${SSH_AGENT_PID}" >>/dev/null 2>&1
}

__add_ssh_identities() {
  __check_ssh_agent_loaded
  if [ "$?" = 1 ]; then
    log_info "Adding SSH identities..."
    ssh-add
  fi
}

__print_error_message_could_not_start_ssh_agent() {
    err "Could not start SSH agent."
    cat <<EOF
Try manually starting the agent by running these commands:
  eval \$(ssh-agent)
  ssh-add
EOF
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
