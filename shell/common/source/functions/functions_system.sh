#!/bin/sh

# An alias function for invoking our preferred system monitoring command.
sysmon() {
  # Default to `top` if nothing else is available.
  local systemMonitorCommand="top"

  if [ -x "$(command -v glances)" ]; then
    # Glances is best
    systemMonitorCommand="glances"
  elif [ -x "$(command -v htop)" ]; then
    # htop is a classic alternative
    systemMonitorCommand="htop"
  fi

  log_debug "Invoking system monitor: ${systemMonitorCommand}"
  eval "${systemMonitorCommand}"
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
