#!/usr/bin/env zsh

###########################################################################
##                                                                       ##
##                            Handy Functions                            ##
##                                                                       ##
###########################################################################

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


ZSH_SPECTRUM_TEXT=${ZSH_SPECTRUM_TEXT:-Arma virumque cano Troiae qui primus ab oris}

# Show all 256 colors with color number
spectrum_ls () {
  for code in {000..255}; do
    print -P -- "$code: %{$FG[$code]%}$ZSH_SPECTRUM_TEXT%{$reset_color%}"
  done
}

# Show all 256 colors where the background is set to specific color
spectrum_bls () {
  for code in {000..255}; do
    print -P -- "$code: %{$BG[$code]%}$ZSH_SPECTRUM_TEXT%{$reset_color%}"
  done
}


###########################################################################
##                                                                       ##
##                             Color Codes                               ##
##                                                                       ##
###########################################################################

export RED=$fg[red]
export ORANGE=$fg[orange]
export CYAN=$fg[cyan]
export YELLOW=$fg[yellow]
export BLUE=$fg[blue]
export LAVENDER=$fg[lavender]
export GREEN=$fg[green]
export PURPLE=$fg[purple]
export MAGENTA=$fg[magenta]
export BLACK=$fg[black]
export WHITE=$fg[white]




