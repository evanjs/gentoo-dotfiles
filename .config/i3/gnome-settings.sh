#!/usr/bin/env sh

# Terminate already running bar instances
if pgrep -x gnome-settings-daemonlocaleexec >/dev/null; then
    sleep 1;
else
    /usr/libexec/gnome-settings-daemon-localeexec
fi
