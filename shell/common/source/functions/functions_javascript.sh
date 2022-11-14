#!/bin/sh

# Deno is a modern JavaScript, TypeScript, and WebAssembly runtime for Windows,
# macOS, and Linux. It's main selling point is that it's very secure. It comes
# with a number of developer tools, including a very fast language server.
install_deno() {
  local denoInstallUrl="https://deno.land/x/install/install.sh"

  curl -fsSL "${denoInstallUrl}" | sh
}

# vim:foldenable:foldmethod=indent:foldnestmax=1