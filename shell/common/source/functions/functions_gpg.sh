#!/bin/sh

# NOTE: Many of the functions in this file are obsolete, and only remain for
#       backwards compatibility with older versions of `gpg`. New versions of
#       `gpg` start `gpg-agent` automatically and manage the agent's lifecycle
#       for you, making the use of these functions unnecessary. Older versions
#       did require you to start the agent manually, however, and it is with
#       these older versions of `gpg` in mind that I have decided to keep these
#       functions around. Because of the forward thinking people pitching in for
#       `gpg` development, using these functions in newer versions of `gpg` does
#       essentially nothing, and at the very least causes no harm. Thus, there
#       is no reason to get rid of them.

# Communicate with a running gpg-agent, or start a new instance if none are
# currently running.
gpgagent() {
  local gpgAgentFile="${HOME}/.gpg-agent-info"

  # NOTE: Following the same logic as for SSH agents, we do not want to create
  #       new GPG agents if we log in over SSH. We want our locally running GPG
  #       agent to do all of the heavy lifting, and we can leverage agent
  #       forwarding over SSH to make use of a locally running agent.
  if [ -n "${SSH_TTY}" ]; then
    warn "Shell running in SSH terminal. Not loading GPG agent."
    return 0
  fi

  if [ -f "${gpgAgentFile}" ]; then
    __load_existing_gpg_agent_from_file

    if ! __is_gpg_agent_running; then
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

__is_gpg_agent_running() {
  pgrep gpg-agent >>/dev/null 2>&1
}

__load_existing_gpg_agent_from_file() {
  local gpgAgentFile="${HOME}/.gpg-agent-info"

  log_info "Reading GPG agent information from file:" \
    "${BLUE}${gpgAgentFile}${NC}"
  . "${gpgAgentFile}"
  export GPG_AGENT_INFO
  export SSH_AUTH_SOCK
}

# Construct a new instance of gpg-agent and write its environment properties to
# a file for future reference. This avoids creating unnecessary duplicate
# gpg-agents in new terminals.
__create_gpg_agent() {
  local gpgAgentFile="${HOME}/.gpg-agent-info"

  log_info "Starting GPG agent and writing its configuration to file:" \
    "${BLUE}${gpgAgentFile}${NC}"
  # NOTE: Newer versions of `gpg-agent` do not write the env file. There is no
  #       reason to keep an env file since `gpg` manages the agent's lifecycle
  #       for you nowadays. The '--write-env-file' option remains for
  #       compatibility reasons only and is ignored on the command line.
  gpg-agent --daemon --enable-ssh-support \
    --write-env-file "${gpgAgentFile}" >>/dev/null 2>&1
}

# Should always export GPG_TTY! This sets the current terminal as the one to
# receive GPG information.
__set_current_tty_for_gpg() {
  GPG_TTY="$(tty)"
  log_info "Setting current TTY for GPG input/output: ${CYAN}${GPG_TTY}${NC}"
  export GPG_TTY
}

# Symlink the gpg.conf file in the dotfiles repo to ~/.gnupg/gpg.conf
link_gpg_config() {
  local gpgConfFile="${DOTFILES_LINK}/gnupg/gpg.conf"
  local gnupgHome="${HOME}/.gnupg"
  local gpgConfTarget="${gnupgHome}/gpg.conf"

  if [ ! -f "${gpgConfFile}" ]; then
    err "No GPG configuration file found at: ${BLUE}${gpgConfFile}${NC}"
    return 1
  fi

  log_info "Linking ${BLUE}${gpgConfFile}${NC} to ${BLUE}${gpgConfTarget}${NC}"
  mkdir --mode=700 "${gnupgHome}"
  ln -sf "${gpgConfFile}" "${gpgConfTarget}"
}

# Print all GPG secret key IDs.
gpgsecret() {
  gpg --list-secret-keys --keyid-format long |
    grep -E '^\s*sec\s+' |
    awk '{print $2}' |
    sed 's|.*/||'
}

# Export a GPG secret key in ASCII armor format.
gpgexport() {
  local secretKeyId="${1}"

  if [ -z "${secretKeyId}" ]; then
    secretKeyId="$(gpgsecret | head -1)"
  fi

  gpg --armor --export "${secretKeyId}"
}

# Encrypt a file using symmetric encryption.
gpgsymenc() {
  local fileName="${1}"
  local passphrase="${2}"
  local encryptedFileName="${3:-${fileName}.gpg}"

  if [ -z "${fileName}" ]; then
    err "No filename provided for encryption"
    return 1
  elif [ -z "${passphrase}" ]; then
    err "No passphrase provided"
    return 2
  fi

  if [ -f "${encryptedFileName}" ]; then
    mv -f "${encryptedFileName}" "${fileName}.bak"
  fi

  gpg --batch \
    --output "${encryptedFileName}"  \
    --passphrase "${passphrase}" \
    --symmetric "${fileName}"
}

# Decrypt a file previously encrypted using symmetric encryption.
gpgsymdec() {
  local encryptedFileName="${1}"
  local passphrase="${2}"
  local fileName="${3:-${encryptedFileName%.gpg}}"

  if [ -z "${encryptedFileName}" ]; then
    err "No filename provided for decryption"
    return 1
  elif [ -z "${passphrase}" ]; then
    err "No passphrase provided"
    return 2
  fi

  if [ -f "${fileName}" ]; then
    mv -f "${fileName}" "${fileName}.bak"
  fi

  gpg --batch \
    --output "${fileName}"  \
    --passphrase "${passphrase}" \
    --decrypt "${encryptedFileName}"
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
