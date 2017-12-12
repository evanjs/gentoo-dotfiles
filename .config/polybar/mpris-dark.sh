#!/bin/bash

# Specifying the icon(s) in the script
# This allows us to change its appearance conditionally
icon=""

player_status=$(playerctl status 2> /dev/null)

if [[ $player_status == *"No players found"*  ]]; then
        exit;
fi;


if [[ $? -eq 0 ]]; then
    metadata="$(playerctl metadata artist) - $(playerctl metadata title) - $(playerctl metadata album)"
fi

# Foreground color formatting tags are optional
if [[ $player_status = "Playing" ]]; then
    echo "%{F#454545}$icon  $metadata"       # Blue when playing
elif [[ $player_status = "Paused" ]]; then
    echo "%{F#454545}$icon  $metadata"       # Green out info when paused
else
    echo "%{F#454545}$icon "                 # Red icon when stopped
fi
