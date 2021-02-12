#!/usr/bin/env bash

# Very useful for testing changes to the rc.lua file.
test_awesome() {
  # An awesome config file can be passed in as the first argument.
  local configFile="${1:-${HOME}/.config/awesome/rc.lua}"
  # The X $DISPLAY value for the nested X session window.
  local newDisplay=":5"

  Xephyr "$newDisplay" -ac -br -noreset -screen 1400x720 &
  sleep 1
  DISPLAY="$newDisplay" awesome --config "${configFile}"
}

# vim:foldenable:foldmethod=indent::foldnestmax=1
