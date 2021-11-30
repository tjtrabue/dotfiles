#!/usr/bin/env bash

# This script runs the xrandr utility to enable additional monitors.
#
# NOTE: You may need to adjust the arguments to --output, as the names of your
# output devices may differ. Use 'xrandr -q' to get a list of your output
# devices and their available modes.

# Output (i.e., monitor) name variables
main_monitor="eDP-1"
second_monitor="DP-1-2-2"
third_monitor="DP-1-2-3"

# Configure the monitors
xrandr --verbose --output "${main_monitor}" --auto --primary \
  --output "${second_monitor}" --mode "2560x1440" --left-of "${main_monitor}" \
  --output "${third_monitor}" --mode "2560x1440" --right-of "${main_monitor}"

# xrandr --verbose --output eDP-1 --auto --primary \
#   --output DP-1-2-2 --auto --left-of eDP-1 \
#   --output DP-1-2-3 --auto --right-of eDP-1
