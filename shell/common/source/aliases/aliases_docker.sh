#!/bin/sh

# Remove stopped containers
alias dcprune="docker container prune"
# Remove unused containers, networks, and images
alias dsprune="docker system prune"

# vim:foldenable:foldmethod=marker:foldlevel=0
