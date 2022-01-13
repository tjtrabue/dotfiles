#!/usr/bin/env zsh

# Like our git aliases, zsh needs no special instruction as to how it should
# complete command aliases when TAB is pressed. Zsh automatically completes
# aliases as it would the original command.

alias d="docker"
alias dat="docker attach"
alias dbu="docker build"
alias dcp="docker cp"
alias ddi="docker diff"
alias dh="docker help"
alias dim="docker image"
alias dims="docker images"
alias din="docker info"
alias dins="docker inspect"
alias dk="docker kill"
alias dlogin="docker login"
alias dlogout="docker logout"
alias dlo="docker logs"
alias dpa="docker pause"
alias dps="docker ps"
alias dpl="docker pull"
alias dre="docker restart"
alias drn="docker run"
alias dst="docker stop"
alias dt="docker tag"
alias dup="docker update"
alias dun="docker unpause"

# docker-compose {{{
if docker compose version >>/dev/null 2>&1; then
  # Use newer `docker compose` when possible. `compose` is now a plugin for
  # Docker, instead of a stand-alone executable.
  alias dp="docker compose"
else
  # Fall back on the standard `docker-compose` command if necessary.
  alias dp="docker-compose"
fi
# }}}

# vim:foldenable:foldmethod=marker:foldlevel=0
