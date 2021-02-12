#!/usr/bin/env bash

# Connect to a WiFi network.
# $1 - The network's SSID (i.e., name)
# $2 - The network's password
netcon() {
  # The "name" of the network to which you wish to connect
  local ssid="$1"
  # The network's password
  local password="$2"

  if [ -z "${ssid}" ]; then
    err "No SSID provided"
    return 1
  elif [ -z "${password}" ]; then
    err "No network password provided"
    return 2
  fi

  case "${NETWORK_TOOL}" in
  "nmcli")
    nmcli device wifi connect "${ssid}" password "${password}"
    ;;
  *)
    err "No networking tool installed"
    ;;
  esac
}

# List all available networks, their strengths, etc.
netlist() {
  case "${NETWORK_TOOL}" in
  "nmcli")
    nmcli device wifi list
    ;;
  *)
    err "No networking tool installed"
    ;;
  esac
}

# List known networks, as well as the currently connected network.
netshow() {
  case "${NETWORK_TOOL}" in
  "nmcli")
    nmcli connection show
    ;;
  *)
    err "No networking tool installed"
    ;;
  esac
}

# Connect to an already-known network
# (that is, one to which you have already connected).
# $1 - The network's SSID (i.e., name)
netup() {
  local ssid="$1"

  case "${NETWORK_TOOL}" in
  "nmcli")
    nmcli connection up "${ssid}"
    ;;
  *)
    err "No networking tool installed"
    ;;
  esac
}

# Private function to retrieve the best available networking tool
# installed on this machine.
__get_network_tool() {
  if [ -n "$(command -v nmcli)" ]; then
    NETWORK_TOOL="nmcli"
  fi

  export NETWORK_TOOL
}
__get_network_tool

# vim:foldenable:foldmethod=indent::foldnestmax=1
