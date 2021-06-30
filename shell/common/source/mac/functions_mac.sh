# Alias a GNU version of a command line tool to its standard name to avoid
# needing to prefix all standard commands with "g".
create_gnu_alias_for_mac_cli_cmd() {
  # The alias for the GNU command.
  local aliasName="${1}"
  # The GNU command to alias.
  local gnuCmd="${2}"

  # Make sure the user provides the required arguments.
  if [ -z "${gnuCmd}" ]; then
    err "No GNU command provided."
    return 1
  elif [ -z "${aliasName}" ]; then
    err "No alias name provided."
    return 2
  fi

  # Create the alias.
  if [ -x "$(command -v "${gnuCmd}")" ]; then
    eval "alias ${aliasName}=\"${gnuCmd}\""
  else
    err "Command ${gnuCmd} not found on \$PATH."
    return 3
  fi
}

# macOS uses BSD versions of UNIX command line tools by default. The GNU
# versions are much more powerful and up-to-date, so we want to use them
# instead.
create_all_gnu_aliases_for_mac() {
  create_gnu_alias_for_mac_cli_cmd "awk" "gawk"
  create_gnu_alias_for_mac_cli_cmd "ln" "gln"
}
