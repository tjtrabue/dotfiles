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

# Escapes an input string to be used on the shell's command line.
escapestr() {
  local input="$*"
  local escapeCmd='s/[^a-zA-Z0-9,._+@%/-]/\\&/g; 1{$s/^$/""/}; 1!s/^/"/; $!s/$/"/'
  local line

  # The horrible sed command adds backslashes to escape the path string
  # appropriately. See this StackOverflow post for more details:
  # https://stackoverflow.com/questions/15783701/which-characters-need-to-be-escaped-when-using-bash/20053121#20053121
  if [ -n "${input}" ]; then
    printf '%s\n' "${input}" | LC_ALL=C sed -e "${escapeCmd}"
  else
    # Read input from stdin if no input provided
    while IFS="" read -r line || [ -n "${line}" ]; do
      printf '%s\n' "${line}" | LC_ALL=C sed -e "${escapeCmd}"
    done </dev/stdin
  fi
}

# Expand shell variables in an input string, or read inputs from stdin.
# This is a more reliable method for ensuring that variables embedded in strings
# come out evaluated.
expandstr() {
  local input="${1}"
  local line

  if [ -n "${input}" ]; then
    printf '%s\n' "$(eval "printf '%s' \"${input}\"")"
  else
    # Read strings from stdin
    while read -r line || [ -n "${line}" ]; do
      printf '%s\n' "$(eval "printf '%s' \"${line}\"")"
    done </dev/stdin
  fi
}

# vim:foldenable:foldmethod=indent::foldnestmax=1
