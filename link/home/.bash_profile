#!/usr/bin/env bash

# SSH Config {{{
SSH_ENV="$HOME/.ssh/environment";
start_agent() {
  if [[ "$(command -v ssh-agent)" != "" ]]; then
    echo "Initializing new SSH agent..." 1>&2;
    /usr/bin/ssh-agent | sed 's/^echo/#echo/' > "${SSH_ENV}";
    echo "Success!" 1>&2;
    chmod 600 "${SSH_ENV}";
    source "${SSH_ENV}" > /dev/null;
    /usr/bin/ssh-add;
  else
    echo "ERROR: No ssh-agent command!" 1>&2;
    return 1;
  fi
}

if [[ -f "${SSH_ENV}" ]]; then
  # If there is an existing environment file, source it so as not to start superfluous agents.
  source "${SSH_ENV}";
  ps -ef | grep "${SSH_AGENT_PID}" | grep ssh-agent$ > /dev/null || {
    start_agent;
  }
else
  # Otherwise, start a new agent and cache identity.
  start_agent;
fi
# }}}

[[ -s "$HOME/.bashrc" ]] && source "$HOME/.bashrc";

# Allow autocomplete for rbenv
if [[ "$(command -v rbenv)" != "" ]]; then
  eval "$(rbenv init -)";
fi
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
