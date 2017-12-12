#!/usr/bin/env sh

BAR=barr
BOTTOM=bottom

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -x polybar >/dev/null; do sleep 1; done

# Launch polybar
if type "xrandr"; then
  for m in $(xrandr --query | grep " connected" | cut -d" " -f1); do
    MONITOR=$m polybar --reload $BAR &
    MONITOR=$m polybar --reload $BOTTOM &
  done
else
  polybar --reload $BAR &
  polybar --reload $BOTTOM &
fi
