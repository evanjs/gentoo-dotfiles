#!/usr/bin/env sh

# Terminate already running bar instances
killall -q polychromatic

# Wait until the processes have been shut down
while pgrep -x polychromatic >/dev/null; do sleep 1; done

# Launch polychromatic
polychromatic-tray-applet
