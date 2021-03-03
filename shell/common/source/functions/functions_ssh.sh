#!/bin/sh

sshagent() {
  # NOTE: We do not want to manipulate running agents if we're connected to a
  #       remote host over SSH! Instead, we should take advantage of agent
  #       forwarding in such scenarios.
  if [ -n "${SSH_TTY}" ]; then
    warn "Shell running in SSH terminal. Not starting an SSH agent."
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

  if ! __check_ssh_agent_running; then
    __print_error_message_could_not_start_ssh_agent
    return 1
  fi

  log_info "SSH agent running with PID: ${SSH_AGENT_PID}"
  __add_ssh_identities
  __remove_other_ssh_agents
}

# Fix SSH auth socket location so agent forwarding works with tmux.
fix_ssh_auth_sock() {
  local mySshAuthSock="${HOME}/.ssh/ssh_auth_sock"

  # Only bother if we are logged in over SSH.
  if [ -n "${SSH_TTY}" ]; then
    log_info "Examining SSH auth socket for validity..."

    # Delete old ssh_auth_sock symlink it if is out of date.
    if [ -S "${SSH_AUTH_SOCK}" ] &&
      [ -e "${mySshAuthSock}" ] &&
      [ "${SSH_AUTH_SOCK}" != "${mySshAuthSock}" ]; then
      log_info "Removing old SSH auth socket file ${mySshAuthSock}"
      rm -f "${mySshAuthSock}"
    fi

    if [ -S "${SSH_AUTH_SOCK}" ] && [ ! -e "${mySshAuthSock}" ]; then
      log_info "Linking real SSH auth sock ${SSH_AUTH_SOCK} to ${mySshAuthSock}"
      ln -sf "${SSH_AUTH_SOCK}" "${mySshAuthSock}"
    fi
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
  if [ -n "${CREATING_SSH_AGENT}" ] && [ "${CREATING_SSH_AGENT}" -ne 0 ]; then
    # We want to treat this variable like a mutex for this function. If it is
    # set to 1, no other processes should attempt to create a new SSH agent.
    export CREATING_SSH_AGENT=1
    local sshAgentFile="${SSH_AGENT_FILE:-${HOME}/.ssh-agent}"
    log_info "Starting new SSH agent..."
    (
      umask 066
      ssh-agent >"${sshAgentFile}"
    )
    export CREATING_SSH_AGENT=0
  else
    log_info "Other process currently creating SSH agent."
  fi
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

# Kill all SSH agents except for the one listed as active in the agent file.
__remove_other_ssh_agents() {
  if [ -n "${SSH_AGENT_PID}" ] &&
    pgrep "ssh-agent" | grep -v "${SSH_AGENT_PID}" >>/dev/null; then
    log_info "Killing non-active SSH agents..."
    pgrep "ssh-agent" | grep -v "${SSH_AGENT_PID}" | xargs kill >>/dev/null 2>&1
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
