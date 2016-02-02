#!/usr/bin/env zsh

# Sources all automation files:
src ()
{
    for file in ~/.zsh-dotfiles/source/* ~/.{vars,dirs}; do
        [ -r "$file" ] && [ -f "$file" ] && source "$file";
    done
}

src