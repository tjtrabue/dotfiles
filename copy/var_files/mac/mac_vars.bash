# macOS {{{

# Homebrew {{{
export BREW_PACKAGES_FILE="${DOTFILES_PACKAGES_DIR}/mac_packages.txt"

# These vars control which C and C++ compilers get used when building
# executables with Homebrew. They should probably stay as clang, clang++ as much
# as possible, but you may need to turn them to gcc, g++ from time to time.
export HOMEBREW_CC="clang"
export HOMEBREW_CXX="clang++"
# }}}

# Homebrew LLVM {{{

# Using the standard LLVM with macOS is annoying. If you are using Xcode and
# have installed the Command Line Developer Tools along with Xcode, then the
# version of Clang that you use is actually the one that comes with Xcode.
# If you want to use the version of Clang installed via Homebrew, you need to
# uncomment the following two lines:
# export LDFLAGS="-L/usr/local/opt/llvm/lib -Wl,-rpath,/usr/local/opt/llvm/lib"
# export CPPFLAGS="-I/usr/local/opt/llvm/include"

# }}}
# }}}