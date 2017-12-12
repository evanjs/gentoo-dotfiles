#!/usr/bin/env sh

# Terminate already running bar instances
killall -q slack 

# Wait until the processes have been shut down
while pgrep -x slack >/dev/null; do sleep 1; done

# Launch slack
i3-msg 'workspace 2; exec slack; workspace 3'
assign [class="Slack"] $WS7
