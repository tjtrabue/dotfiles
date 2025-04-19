#!/bin/sh

# This is the Polybar launch script. Invoke this script from your window manager
# instead of simply calling "polybar &".

polybarName="tjtrabue"

# Terminate already running bar instances
killall -q polybar
# If all your bars have ipc enabled, you can also use
# polybar-msg cmd quit

# Launch Polybar, using default config location ~/.config/polybar/config.ini
polybar "${polybarName}" 2>&1 | tee -a /tmp/polybar.log &
disown

# Duplicate polybar across multiple monitors
if [ -n "$(command -v xrandr)" ]; then
  for m in $(xrandr --query | grep " connected" | cut -d" " -f1); do
    MONITOR=$m polybar --reload "${polybarName}" &
  done
else
  polybar --reload "${polybarName}" &
fi

unset polybarName

printf "%s\n" "Polybar launched..."
