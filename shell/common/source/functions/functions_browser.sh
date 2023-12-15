#!/bin/sh

# Launch Brave browser with additional features enabled.
brave() {
  braveExec="$(command -v brave-beta)"
  braveFlags="--enable-accelerated-2d-canvas --enable-gpu-rasterization --enable-skia-graphite --enable-unsafe-webgpu"

  if [ ! -x "${braveExec}" ]; then
    braveExec="$(command -v brave)"
  fi

  eval "${braveExec} ${braveFlags}"
}

# vim:foldenable:foldmethod=indent:foldnestmax=1