#!/usr/bin/env sh

# Terminate already running bar instances
if pgrep -x gnome-settings-daemonlocaleexec >/dev/null; then
    sleep 1;
else
    # Restore GNOME's settings
    exec --no-startup-id /usr/libexec/gnome-settings-daemon-localeexec

    # Fix a bug in gnome-settings-daemon: http://feeding.cloud.geek.nz/posts/creating-a-modern-tiling-desktop-environment-using-i3/
    exec --no-startup-id dconf write /org/gnome/settings-daemon/plugins/cursor/active false
fi
