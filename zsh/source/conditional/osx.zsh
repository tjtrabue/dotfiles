#!/usr/bin/env zsh

# This file contains aliases and functions specific to Apple's OS X operating system.
# Abort if not OSX
is_osx || return 1

# Make 'less' more
[[ "$(type lesspipe.sh)" ]] && eval "$(lesspipe.sh)";

# Trim new lines and copy to clipboard
alias c="tr -d '\n' | pbcopy";

# Start ScreenSaver. This will lock the screen if locking is enabled.
alias ss="
open /System/Library/Frameworks/ScreenSaver.framework/Versions/A/Resources/ScreenSaverEngine.app"