#!/usr/bin/env zsh

# Printing {{{
alias echo="echo -e"
alias lsa="ls -A"
alias lsh="ls -A | egrep '^\.'"
# }}}

# Deleting {{{
alias rmr="rm -r"
alias rmrf="rm -rf"
# }}}

# Searching {{{
alias grepr="grep -G -r";
alias egrepr="grep -E -r";
alias fgrepr="grep -F -r";
alias greppr="grep -P -r";

# Force ag to always use global .agignore file when searching
alias ag="ag --path-to-ignore ~/.agignore";
# }}}

# Sourcing {{{
alias szshrc="source $HOME/.zshrc";
alias sdirs="source $DIR_ALIAS_FILE"
# }}}

# tmux {{{
# Force tmux to use 256 colors by default
alias tmux="tmux -2";

alias tx="tmux";

alias txa="tmux attach-session";
alias txd="tmux detach-client";

alias txns="tmux new-session";
alias txnw="tmux new-window";

alias txks="tmux kill-session -t";
alias txksr="tmux kill-server";

alias txls="tmux list-sessions";
# }}}

# Vim {{{
# Alias 'edit' as the alias for the default editor
alias edit="nvim";
# Alias 'e' as an 'edit' alias
alias e="edit";

# Make 'v' an alias for vim:
alias v="vim";

# Alias vim to neovim if available
if [[ "$(command -v nvim)" != "" ]]; then
  alias vim="nvim";
fi

# Aliases for opening particular files:
alias vzrc="edit $HOME/.zshrc";
alias vzenv="edit $HOME/.zshenv";
alias vvrc="edit $HOME/.vimrc";
alias vivrc="edit $HOME/.ideavimrc";
alias vdirs="edit $DIR_ALIAS_FILE";
alias vpath="edit $PATH_FILE";
alias vau="edit $DOT_ZSH/source/aliases_universal.zsh";
alias vag="edit $DOT_ZSH/source/aliases_git.zsh";
alias vfu="edit $DOT_ZSH/source/functions_universal.zsh";
alias vfg="edit $DOT_ZSH/source/functions_git.zsh";
alias vcolors="edit $DOT_ZSH/source/colors.zsh";
alias vgconf="edit $HOME/.gitconfig";
alias vtconf="edit $HOME/.tmux.conf";
alias vosx="edit $DOT_ZSH/source/conditional/osx.zsh";
alias vubuntu="edit $DOT_ZSH/source/conditional/ubuntu.zsh";
alias vsvinit="edit $HOME/.SpaceVim.d/init.vim";

alias install_space_vim="curl -sLf https://spacevim.org/install.sh | bash";
# }}}

# Global Aliases {{{
alias -g gp="| grep -i";
alias -g sd="| sed -E";
# }}}

# mathiasbynens/dotfiles Aliases {{{
###########################################################################
##                          Aliases taken from                           ##
##                        mathiasbynens/dotfiles                         ##
##               https://github.com/mathiasbynens/dotfiles               ##
###########################################################################

# Easier navigation: .., ..., ...., ....., ~ and -
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias ~="cd ~" # `cd` is probably faster to type though
alias -- -="cd -"

# Shortcuts
alias g="git"
alias h="history"
alias j="jobs"

# Detect which `ls` flavor is in use
if ls --color > /dev/null 2>&1; then # GNU `ls`
    colorflag="--color"
else # OS X `ls`
    colorflag="-G"
fi

# List all files colorized in long format
alias l="ls -lFh ${colorflag}"

alias ls="ls -GFh"

# List all files colorized in long format, including dot files
alias la="ls -laFh ${colorflag}"

# List only directories
alias lsd="ls -lF ${colorflag} | grep --color=never '^d'"

# Always use color output for `ls`
alias ls="command ls ${colorflag}"

# Always enable colored `grep` output
# Note: `GREP_OPTIONS="--color=auto"` is deprecated, hence the alias usage.
alias grep='grep --color=auto'
alias egrep='grep -E --color=auto'
alias fgrep='grep -F --color=auto'

# Enable aliases to be sudo’ed
alias sudo='sudo '

# Get week number
alias week='date +%V'

# Stopwatch
alias timer='echo "Timer started. Stop with Ctrl-D." && date && time cat && date'

# Get OS X Software Updates, and update installed Ruby gems, Homebrew, npm, and their installed packages
alias update='sudo softwareupdate -i -a; brew update; brew upgrade --all; brew cleanup; npm install npm -g; npm update -g; sudo gem update --system; sudo gem update'

# IP addresses
alias ip="dig +short myip.opendns.com @resolver1.opendns.com"
alias localip="ipconfig getifaddr en0"
alias ips="ifconfig -a | grep -o 'inet6\? \(addr:\)\?\s\?\(\(\([0-9]\+\.\)\{3\}[0-9]\+\)\|[a-fA-F0-9:]\+\)' | awk '{ sub(/inet6? (addr:)? ?/, \"\"); print }'"

# Flush Directory Service cache
alias flush="dscacheutil -flushcache && killall -HUP mDNSResponder"

# Clean up LaunchServices to remove duplicates in the “Open With” menu
alias lscleanup="/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user && killall Finder"

# View HTTP traffic
alias sniff="sudo ngrep -d 'en1' -t '^(GET|POST) ' 'tcp and port 80'"
alias httpdump="sudo tcpdump -i en1 -n -s 0 -w - | grep -a -o -E \"Host\: .*|GET \/.*\""

# Canonical hex dump; some systems have this symlinked
command -v hd > /dev/null || alias hd="hexdump -C"

# OS X has no `md5sum`, so use `md5` as a fallback
command -v md5sum > /dev/null || alias md5sum="md5"

# OS X has no `sha1sum`, so use `shasum` as a fallback
command -v sha1sum > /dev/null || alias sha1sum="shasum"

# JavaScriptCore REPL
jscbin="/System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources/jsc";
[ -e "${jscbin}" ] && alias jsc="${jscbin}";
unset jscbin;

# Trim new lines and copy to clipboard
alias c="tr -d '\n' | pbcopy"

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
command -v grunt > /dev/null && alias grunt="grunt --stack"

# Stuff I never really use but cannot delete either because of http://xkcd.com/530/
alias stfu="osascript -e 'set volume output muted true'"
alias pumpitup="osascript -e 'set volume 7'"

# Kill all the tabs in Chrome to free up memory
# [C] explained: http://www.commandlinefu.com/commands/view/402/exclude-grep-from-your-grepped-output-of-ps-alias-included-in-description
alias chromekill="ps ux | grep '[C]hrome Helper --type=renderer' | grep -v extension-process | tr -s ' ' | cut -d ' ' -f2 | xargs kill"

# Lock the screen (when going AFK)
alias afk="/System/Library/CoreServices/Menu\ Extras/User.menu/Contents/Resources/CGSession -suspend"

# Reload the shell (i.e. invoke as a login shell)
alias reload="exec $SHELL -l"
# }}}

# Modeline for this file (LEAVE IT COMMENTED!)
# vim:foldmethod=marker
