#!/bin/sh

# Run maven with default command line parameters.
# Check ~/.vars for more information.
alias mvn='command mvn $(eval "echo "${MAVEN_DEFAULT_PARAMS}"")'

# vim:foldenable:foldmethod=marker:foldlevel=0
