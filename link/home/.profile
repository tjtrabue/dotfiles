#!/bin/sh

# Make sure that all sessions have access to user's executables
NEW_PATH="${HOME}/bin"
NEW_PATH="${HOME}/.local/bin:${NEW_PATH}"
PATH="${NEW_PATH}:${PATH}"
export PATH
unset NEW_PATH
