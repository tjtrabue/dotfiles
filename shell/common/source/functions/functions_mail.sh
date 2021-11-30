#!/bin/sh

# Use Mutt/Neomutt depending on what's installed,
# and open in attachments directory if available.
mutt() {
  local muttCommand

  muttCommand="$(command -v mutt)"

  if [ "$(command -v neomutt)" != "" ]; then
    muttCommand="$(command -v neomutt)"
  fi

  if [ -z "$muttCommand" ]; then
    err "Could not find mutt or neomutt executable on \$PATH."
    return 1
  fi

  if [ -d "$MUTT_ATTACHMENTS_DIR" ]; then
    pushd "$MUTT_ATTACHMENTS_DIR" &>/dev/null || return 2
    eval "$muttCommand"
    popd &>/dev/null || return 2
  else
    warn "No attachments directory found"
    eval "$muttCommand"
  fi
}
