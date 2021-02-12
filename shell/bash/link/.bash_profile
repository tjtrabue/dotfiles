#!/usr/bin/env bash

[[ -s ~/.bashrc ]] && source ~/.bashrc

# Allow autocomplete for rbenv
eval "$(rbenv init -)"

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/Users/tom/.sdkman"
[[ -s "/Users/tom/.sdkman/bin/sdkman-init.sh" ]] && source "/Users/tom/.sdkman/bin/sdkman-init.sh"