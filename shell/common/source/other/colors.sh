#!/usr/bin/env bash

# Turn off coloration
export NC="$(tput sgr0)"

# Standard colors
export BLACK="$(tput setaf 0)"
export RED="$(tput setaf 1)"
export GREEN="$(tput setaf 2)"
export YELLOW="$(tput setaf 3)"
export BLUE="$(tput setaf 4)"
export MAGENTA="$(tput setaf 5)"
export CYAN="$(tput setaf 6)"
export WHITE="$(tput setaf 7)"

# Background colors
export BLACK_BG="$(tput setab 0)"
export RED_BG="$(tput setab 1)"
export GREEN_BG="$(tput setab 2)"
export YELLOW_BG="$(tput setab 3)"
export BLUE_BG="$(tput setab 4)"
export MAGENTA_BG="$(tput setab 5)"
export CYAN_BG="$(tput setab 6)"
export WHITE_BG="$(tput setab 7)"

# Print all basic, 256 terminal colors and their escape sequences for reference:
colortest() {
  local x a i
  for x in {0..8}; do
    for i in {30..37}; do
      for a in {40..47}; do
        echo -e "\e[${x};${i};${a}""m\\\e[${x};${i};${a}""m\e[0;37;40m"
      done
    done
  done | column -c $(($(tput cols) * 2))
}

colortest256() {
  local x
  for x in {0..255}; do
    echo -e "\033[48;5;${x}m${x}\e[0m"
  done | column -c $(($(tput cols) * 2))
}

# Test the full 24-bit true color spectrum. Only works in terminals that support true colors! PuTTY
# will not like this function!
truecolortest() {
  local width="$(tput cols || echo 80)"
  awk -v term_cols="${width}" 'BEGIN{
    s="/\\";
    for (colnum = 0; colnum<term_cols; colnum++) {
      r = 255-(colnum*255/term_cols);
      g = (colnum*510/term_cols);
      b = (colnum*255/term_cols);
      if (g>255) g = 510-g;
      printf "\033[48;2;%d;%d;%dm", r,g,b;
      printf "\033[38;2;%d;%d;%dm", 255-r,255-g,255-b;
      printf "%s\033[0m", substr(s,colnum%s+1,1);
    }
    printf "\n";
  }'
}

# vim:foldmethod=marker:
