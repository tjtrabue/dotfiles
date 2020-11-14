#!/bin/sh

# Make sure that all sessions have access to user's executables
PATH="${HOME}/bin:${PATH}"
PATH="${HOME}/.local/bin:${PATH}"
export PATH