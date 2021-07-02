#!/bin/sh

# Alias function for invoking a CLI directory navigation tool the user
# specifies.
dirnav() {
  local dirNavTool="ranger-cd"

  if [ -x "$(command -v broot)" ]; then
    # Use broot if possible, since its faster than ranger.
    dirNavTool="br"
  fi

  "${dirNavTool}"
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
