#!/usr/bin/env zsh

# Handy Functions {{{
# Make use of colors:
autoload -U colors && colors

FX=(
    reset     "%{[00m%}"
    bold      "%{[01m%}" no-bold      "%{[22m%}"
    italic    "%{[03m%}" no-italic    "%{[23m%}"
    underline "%{[04m%}" no-underline "%{[24m%}"
    blink     "%{[05m%}" no-blink     "%{[25m%}"
    reverse   "%{[07m%}" no-reverse   "%{[27m%}"
)

for color in {000..255}; do
    FG[$color]="%{[38;5;${color}m%}"
    BG[$color]="%{[48;5;${color}m%}"
done

ZSH_SPECTRUM_TEXT="${ZSH_SPECTRUM_TEXT:-'Test'}";

# Show all 256 colors with color number
spectrum_ls () {
    for code in {000..255}; do
        print -P -- "$code: %{$FG[$code]%}$ZSH_SPECTRUM_TEXT%{$reset_color%}";
    done | column;
}

# Show all 256 colors where the background is set to specific color
spectrum_bls () {
    for code in {000..255}; do
        print -P -- "$code: %{$BG[$code]%}$ZSH_SPECTRUM_TEXT%{$reset_color%}";
    done | column;
}
# }}}

# Colors {{{
### Foreground ###
export NC="$(tput sgr0)";
export BLACK="$(tput setaf 0)";
export RED="$(tput setaf 124)";
export GREEN="$(tput setaf 64)";
export YELLOW="$(tput setaf 136)";
export ORANGE="$(tput setaf 166)";
export BLUE="$(tput setaf 33)";
export MAGENTA="$(tput setaf 125)";
export CYAN="$(tput setaf 37)";
export VIOLET="$(tput setaf 61)";
export WHITE="$(tput setaf 15)";

### Font face ###
export BOLD_FF=$(tput bold);
# }}}

# Modeline for this file (LEAVE IT COMMENTED!)
# vim:foldmethod=marker
