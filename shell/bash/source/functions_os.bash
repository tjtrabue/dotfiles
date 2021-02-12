#!/usr/bin/env bash

# Gets the Linux OS's name and version.
getosinfo() {
  # Figure out the distro based on one of a few things.
  if [ -f /etc/os-release ]; then
    # freedesktop.org and systemd
    source /etc/os-release;
    OS=$NAME;
    VER=$VERSION_ID;
  elif type lsb_release >/dev/null 2&>1; then
    # linuxbase.org
    OS="$(lsb_release -si)";
    VER="$(lsb_release -sr)";
  elif [ -f /etc/lsb-release ]; then
    # For some versions of Debian/Ubuntu without lsb_release command
    source /etc/lsb-release;
    OS="$DISTRIB_ID";
    VER="$DISTRIB_RELEASE";
  elif [ -f /etc/debian_version ]; then
    # Older Debian/Ubuntu/etc.
    OS="Debian";
    VER="$(cat /etc/debian_version)";
  elif [ -f /etc/SuSe-release ]; then
    # Older SuSE/etc.
    OS="SuSE";
    VER="unknown";
  elif [ -f /etc/redhat-release ]; then
    # Older Red Hat, CentOS, etc.
    OS="RedHat";
    VER="unknown";
  else
    OS="$(uname -s)";
    VER="$(uname -r)";
  fi

  # Return the distribution name and version.
  echo "Distribution: ${OS}";
  echo "Version: ${VER}";
}

# Retrieves the system package manager for the current operating system.
getpm() {
  local distId;
  local pm="";

  # Get thr distribution ID:
  distId="$(getdistro | \
    grep '^Distribution:' | \
    sed 's/^Distribution:\s*//' \
  )";

  case "$distId" in
    "CentOS"|"RedHat")
      pm="yum";
      ;;
    "Ubuntu")
      pm="apt-get";
      ;;
    *)
      err "Unknown OS $distId";
      return 1;
  esac

  # Return the name of the package manager:
  echo "$pm";
}

# Gets the Linux distribution name only
getdistro() {
  getosinfo | grep 'Distribution:' | sed 's/Distribution: //';
}

# Gets the Linux distrubtion version only
getosversion() {
  getosinfo | grep 'Version:' | sed 's/Version: //';
}

# vim:foldenable:foldmethod=indent::foldnestmax=1
