#!/bin/sh

# Printing / clearing aliases: {{{
# NOTE: 'ls' aliases are now functions in functions_ls.sh

alias cl="clear"
# }}}

# Delete aliases: {{{
alias rmr="rm -r"
alias rmrf="rm -rf"
# }}}

# Source aliases {{{
alias sbrc="source \${HOME}/.bashrc"
alias szrc="source \${ZDOTDIR}/.zshrc"
alias sbpr="source \${HOME}/.bash_profile"
alias sdirs="source \${HOME}/.dirs"
alias svar="source \${HOME}/.vars"
alias sirc="bind -f \${HOME}/.inputrc"
alias sxre="xrdb -merge \${HOME}/.Xresources"
# }}}

# Find files with CRLF line endings
alias dosfiles="find . -not -type d -exec file '{}' ';' | grep CRLF | grep -o --color=never '^.*: ' | sed 's/..$//'"

# Reload the shell (i.e. invoke as a login shell)
alias reshell="exec \${SHELL} -l"

# Directory syncing aliases:
alias rsa="rsync -a"

# Search aliases {{{
# Change the default behavior of ag
alias ag="ag --hidden"
# }}}

# readline aliases {{{
# These aliases require the rlwrap command, which intercepts key strokes sent to
# the commands and runs them through readline.
# NOTE: not currently using rlwrap aliases since doing so breaks tab completion.
# if [ -x "$(command -v rlwrap)" ]; then
#   # Use readline configuration for Elixir shell, since iex does not have
#   # native support for readline.
#   alias iex="rlwrap -a -A -t dumb iex"
# fi
# }}}

# Copy working directory to clipboard.
alias cwd="pwd | tr -d '\n' | clipboard"

###########################################################################
##                          Aliases taken from                           ##
##                        mathiasbynens/dotfiles                         ##
##               https://github.com/mathiasbynens/dotfiles               ##
###########################################################################

# {{{
# Easier navigation: .., ..., ...., ....., ~ and - {{{
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias ~="cd ~" # `cd` is probably faster to type though
alias -- -="cd -"
# }}}

# Shortcuts {{{
alias j="jobs"
# }}}

# Always enable colored `grep` output
# Note: `GREP_OPTIONS="--color=auto"` is deprecated, hence the alias usage.
alias grep='grep --color=auto'
alias fgrep='grep -F --color=auto'
alias egrep='grep -E --color=auto'

# Enable aliases to be sudo’ed
alias sudo='sudo '

# Get week number
alias week='date +%V'

# Stopwatch
alias timer='echo "Timer started. Stop with Ctrl-D." && date && time cat && date'

# Flush Directory Service cache
alias flush="dscacheutil -flushcache && killall -HUP mDNSResponder"

# Clean up LaunchServices to remove duplicates in the “Open With” menu
alias lscleanup="/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user && killall Finder"

# View HTTP traffic
alias sniff="sudo ngrep -d 'en1' -t '^(GET|POST) ' 'tcp and port 80'"
alias httpdump="sudo tcpdump -i en1 -n -s 0 -w - | grep -a -o -E \"Host\: .*|GET \/.*\""

# Canonical hex dump; some systems have this symlinked
command -v hd >/dev/null || alias hd="hexdump -C"

# OS X has no `md5sum`, so use `md5` as a fallback
command -v md5sum >/dev/null || alias md5sum="md5"

# OS X has no `sha1sum`, so use `shasum` as a fallback
command -v sha1sum >/dev/null || alias sha1sum="shasum"

# JavaScriptCore REPL
jscbin="/System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources/jsc"
[ -e "${jscbin}" ] && alias jsc="${jscbin}"
unset jscbin

# Recursively delete `.DS_Store` files
alias cleanup="find . -type f -name '*.DS_Store' -ls -delete"

# Empty the Trash on all mounted volumes and the main HDD.
# Also, clear Apple’s System Logs to improve shell startup speed.
# Finally, clear download history from quarantine. https://mths.be/bum
alias emptytrash="sudo rm -rfv /Volumes/*/.Trashes; sudo rm -rfv ~/.Trash; sudo rm -rfv /private/var/log/asl/*.asl; sqlite3 ~/Library/Preferences/com.apple.LaunchServices.QuarantineEventsV* 'delete from LSQuarantineEvent'"

# Show/hide hidden files in Finder
alias show="defaults write com.apple.finder AppleShowAllFiles -bool true && killall Finder"
alias hide="defaults write com.apple.finder AppleShowAllFiles -bool false && killall Finder"

# Hide/show all desktop icons (useful when presenting)
alias hidedesktop="defaults write com.apple.finder CreateDesktop -bool false && killall Finder"
alias showdesktop="defaults write com.apple.finder CreateDesktop -bool true && killall Finder"

# URL-encode strings
alias urlencode='python -c "import sys, urllib as ul; print ul.quote_plus(sys.argv[1]);"'

# Merge PDF files
# Usage: `mergepdf -o output.pdf input{1,2,3}.pdf`
alias mergepdf='/System/Library/Automator/Combine\ PDF\ Pages.action/Contents/Resources/join.py'

# Disable Spotlight
alias spotoff="sudo mdutil -a -i off"
# Enable Spotlight
alias spoton="sudo mdutil -a -i on"

# PlistBuddy alias, because sometimes `defaults` just doesn’t cut it
alias plistbuddy="/usr/libexec/PlistBuddy"

# Ring the terminal bell, and put a badge on Terminal.app’s Dock icon
# (useful when executing time-consuming commands)
alias badge="tput bel"

# Intuitive map function
# For example, to list all directories that contain a certain file:
# find . -name .gitattributes | map dirname
alias map="xargs -n1"

# One of @janmoesen’s ProTip™s
for method in GET HEAD POST PUT DELETE TRACE OPTIONS; do
  alias "$method"="lwp-request -m '$method'"
done

# Make Grunt print stack traces by default
command -v grunt >/dev/null && alias grunt="grunt --stack"

# Stuff I never really use but cannot delete either because of http://xkcd.com/530/
alias stfu="osascript -e 'set volume output muted true'"
alias pumpitup="osascript -e 'set volume 7'"

# Kill all the tabs in Chrome to free up memory
# [C] explained: http://www.commandlinefu.com/commands/view/402/exclude-grep-from-your-grepped-output-of-ps-alias-included-in-description
alias chromekill="ps ux | grep '[C]hrome Helper --type=renderer' | grep -v extension-process | tr -s ' ' | cut -d ' ' -f2 | xargs kill"

# Lock the screen (when going AFK)
alias afk="/System/Library/CoreServices/Menu\ Extras/User.menu/Contents/Resources/CGSession -suspend"
# }}}

# vim:foldenable:foldmethod=marker
