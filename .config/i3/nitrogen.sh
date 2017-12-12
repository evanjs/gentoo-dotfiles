#!/usr/bin/env sh

# Terminate already running bar instances
killall -q nitrogen 

# Wait until the processes have been shut down
while pgrep -x nitrogen >/dev/null; do sleep 1; done

# Launch nitrogen
nitrogen --restore
