#!/bin/sh

# Determine the Linux graphical session type. This is usually either X11 or
# Wayland.
get_xdg_session_type() {
  if [ -x "$(command -v loginctl)" ] && loginctl list-sesions >>/dev/null 2>&1; then
    loginctl show-session "$(awk '/tty/ {print $1}' <(loginctl))" -p Type |
      awk -F= '{print $2}' | tr -d '\n'
  fi
}

# Determine the GDK backend in use for GTK applications.
# The return value should be used for the value of the GDK_BACKEND environment
# variable.
get_gdk_backend() {
  local ostype
  ostype="$(getostype)"
  local gdkBackend=""
  gdkBackend="$(get_xdg_session_type)"

  if printf '%s' "${ostype}" | grep -Eqi "darwin"; then
    gdkBackend='macos'
  elif printf '%s' "${ostype}" | grep -Eqi '(cygwin)|(mingw)'; then
    gdkBackend='win32'
  elif [ -n "${WAYLAND_DISPLAY}" ] || [ "${XDG_SESSION_TYPE}" = 'wayland' ]; then
    gdkBackend='wayland'
  fi

  printf "%s" "${gdkBackend}"
}

# vim:foldenable:foldmethod=indent:foldnestmax=1