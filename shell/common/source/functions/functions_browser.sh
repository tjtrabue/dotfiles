#!/bin/sh

# Launch Brave browser with additional features enabled.
brave() {
  braveExec="$(command -v brave-beta)"

  if [ -z "${braveExec}" ]; then
    braveExec="$(command -v brave)"
  fi

  braveFlags="--enable-accelerated-2d-canvas --enable-gpu-rasterization --enable-skia-graphite"

  if [ ! -x "${braveExec}" ]; then
    braveExec="$(command -v brave)"
  fi

  eval "${braveExec} ${braveFlags}"
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
