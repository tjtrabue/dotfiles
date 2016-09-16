#!/bin/zsh

# Saved in ~/online-check.sh and in a cron job as:
# * * * * * ~/online-check.sh

local offline=`dig 8.8.8.8 +time=1 +short google.com A | grep -c "no servers could be reached"`
if [[ "$offline" == "0" ]]; then
  rm ~/.offline
else
  touch ~/.offline
fi