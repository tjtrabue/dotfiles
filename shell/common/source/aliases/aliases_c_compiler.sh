#!/bin/sh

# Print all defined preprocessor macros that can be expanded.
alias gccmacros="echo | gcc -dM -E -"
alias clangmacros="echo | clang -dM -E -"

# vim:foldenable:foldmethod=marker:foldlevel=0