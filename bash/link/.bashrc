#!/usr/bin/env bash

# Where the magic happens.
export DOTFILES_HOME=~/".dotfiles"

# Add binaries into the path
export PATH="$DOTFILES_HOME/bash/bin:$PATH"

# Source all files in "source"
function src() {
    local file
    if [[ "$1" ]]; then
        source "$DOTFILES_HOME/bash/source/$1.bash"
    else
        for file in ~/.{vars,dirs,path}; do
  		    source "$file"
        done
        for file in $DOTFILES_HOME/bash/source/*; do
            source "$file"
        done
        for file in $DOTFILES_HOME/git/source/*; do
            source "$file"
        done
    fi
}

# Git rid of old directory aliases in .dirs that no longer work
function rmdirs() {
    # Move the .dirs file into a temporary location and rewrite it
    mv ~/.dirs ~/.dirs.tmp
    touch ~/.dirs && echo "#!/usr/bin/env bash" >> ~/.dirs
    local line
    while read line; do
        if [[ "$line" =~ ^export.* ]]; then
            local dir="$(echo $line | sed -e 's/^export.*=//' | sed -e 's/"//g')"
            dir="$(eval echo "$dir")"
            if [[ -d "$dir" ]]; then
                echo "$line" >> ~/.dirs
            fi
        fi
    done < ~/.dirs.tmp
    # Remove the temporary dirs file
    rm ~/.dirs.tmp
}

# Add tab completion for many Bash commands:
if [ -f $(brew --prefix)/etc/bash_completion ]; then
    . $(brew --prefix)/etc/bash_completion
fi

rmdirs
src

POWERLINE_HOME=~/".dotfiles/vendors/powerline/"
export PATH="$POWERLINE_HOME/scripts:$PATH"
if [[ -f "$POWERLINE_HOME/powerline/bindings/bash/powerline.sh" ]]; then
    powerline-daemon -q
    POWERLINE_BASH_CONTINUATION=1
    POWERLINE_BASH_SELECT=1
    . "$POWERLINE_HOME/powerline/bindings/bash/powerline.sh"
fi
export PATH="/usr/local/sbin:$PATH"
#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/Users/tom/.sdkman"
[[ -s "/Users/tom/.sdkman/bin/sdkman-init.sh" ]] && source "/Users/tom/.sdkman/bin/sdkman-init.sh"