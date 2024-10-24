# Arch Linux {{{
export AUR_HOME="${HOME}/.aur"
export AUR_PACKAGES_FILE="${DOTFILES_HOME}/init/package_files/aur_packages.txt"

# The user's selected AUR helper program.
export AUR_HELPER="paru"
export AUR_HELPER_BUILD_CMD="-Ui"
export AUR_HELPER_CLEAN_CMD="-Sc"
export AUR_HELPER_GET_CMD="-G"
export AUR_HELPER_INFO_CMD="-Si"
export AUR_HELPER_INSTALL_CMD="-S"
export AUR_HELPER_QUERY_CMD="-Q"
export AUR_HELPER_REMOVE_CMD="-R"
export AUR_HELPER_SEARCH_CMD="-Ss"
export AUR_HELPER_UPDATE_CMD="-Sua"
export AUR_HELPER_NO_CONFIRM_FLAGS="--noconfirm --needed --mflags \
  '--skipchecksums --skippgpcheck'"

# Bootloader
export BOOTLOADER_CFG="/boot/grub/grub.cfg"

# Erlang build configure options
# For some reason, the ODBC lib is in a different location on Arch Linux.
export KERL_CONFIGURE_OPTIONS="--with-odbc=/var/lib/pacman/local/unixodbc-2.3.12-1"
# }}}
