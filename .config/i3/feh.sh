#!/usr/bin/env sh

# Terminate already running bar instances
killall -q feh 

# Wait until the processes have been shut down
while pgrep -x feh >/dev/null; do sleep 1; done

# Launch feh
feh --bg-scale /usr/share/wallpapers/custom/planet12/planet_12-wallpaper-1920x1080.jpg

#wal -i -t -n /usr/share/wallpapers/custom/planet12/planet_12-wallpaper-1920x1080.jpg
