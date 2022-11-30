# macOS {{{

# Homebrew {{{
export BREW_PACKAGES_FILE="${DOTFILES_PACKAGES_DIR}/mac_packages.txt"

# These vars control which C and C++ compilers get used when building
# executables with Homebrew. They should probably stay as clang, clang++ as much
# as possible, but you may need to turn them to gcc, g++ from time to time.
export HOMEBREW_CC="clang"
export HOMEBREW_CXX="clang++"
# }}}

# C/C++ compiler options {{{

# Should be either 'clang' or 'gcc'.
export CC="clang"

if [ "${CC}" = "gcc" ]; then
  # Add header and library paths for GCC.
  export CPATH="$(brew --prefix)/opt/gcc/include/c++/12"
  export LIBRARY_PATH="$(brew --prefix)/opt/gcc/lib/gcc/current"
elif [ -d "$(brew --prefix)/opt/llvm" ]; then
  # Add header and library paths for brew-installed LLVM.
  # These paths are not standard in macOS since new versions of macOS ship with
  # their own version of LLVM.
  export CPATH="$(brew --prefix)/opt/llvm/include"
  export LIBRARY_PATH="$(brew --prefix)/opt/llvm/lib"
fi
# }}}

# Emacs Plus {{{
# Which version of emacs-plus you wish to install
export EMACS_PLUS_VERSION="29"
# }}}
# }}}
