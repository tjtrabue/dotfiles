# Where the magic happens.
export ZSH_DOTFILES_HOME=~/.zsh-dotfiles

# Add binaries into the path
PATH=$ZSH_DOTFILES/bin:$PATH
export PATH

# Source all files in "source"
function src() {
  local file
  if [[ "$1" ]]; then
    source "$ZSH_DOTFILES/bash/source/$1.bash"
  else
    for file in ~/.{vars,dirs,path}; do
			source "$file"
    done

    for file in $ZSH_DOTFILES_HOME/bash/source/*; do
        source "$file"
    done
  fi
}

# Add tab completion for many Bash commands:
if [ -f $(brew --prefix)/etc/bash_completion ]; then
    . $(brew --prefix)/etc/bash_completion
fi

src

POWERLINE_HOME=~/"Dropbox/GitHub/GitHub_Repositories/powerline/"
PATH="$POWERLINE_HOME/scripts:$PATH"
if [[ -f "$POWERLINE_HOME/powerline/bindings/bash/powerline.sh" ]]; then
    powerline-daemon -q
    POWERLINE_BASH_CONTINUATION=1
    POWERLINE_BASH_SELECT=1
    . "$POWERLINE_HOME/powerline/bindings/bash/powerline.sh"
fi
