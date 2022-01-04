#!/bin/sh

# Remove stopped containers
alias dcprune="docker container prune"
# Remove unused containers, networks, and images
alias dsprune="docker system prune"

# docker-compose {{{

# Take down running services and remove dangling containers.
alias dcnuke="docker compose down --remove-orphans --rmi all"
# }}}

# vim:foldenable:foldmethod=marker:foldlevel=0
