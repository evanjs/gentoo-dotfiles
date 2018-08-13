#!/usr/bin/env sh

# Terminate already running bar instances
#killall -q openrazer-daemon
killall -q polychromatic

# Wait until the processes have been shut down
#while pgrep -x openrazer-daemon >/dev/null; do sleep 1; done
while pgrep -x polychromatic >/dev/null; do sleep 1; done

# Launch polychromatic
#openrazer-daemon
dbus-launch polychromatic-tray-applet
