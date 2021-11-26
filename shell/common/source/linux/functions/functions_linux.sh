#!/bin/sh

# Use fuzzy finding tools to quickly adjust the system timezone.
settimezone() {
  local timezone

  if [ -x "$(command -v fzf)" ]; then
    timezone="$(timedatectl list-timezones | fzf)"
  fi

  if [ -z "${timezone}" ]; then
    return 0
  fi

  sudo timedatectl set-timezone "${timezone}"
}

# vim:foldenable:foldmethod=indent::foldnestmax=1
