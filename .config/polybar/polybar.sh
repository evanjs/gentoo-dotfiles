#!/usr/bin/env bash

pkill polybar
polybar -r --config=$HOME/.config/polybar/config barr &
#polybar --config=$HOME/.config/polybar/config-openbox bar2 &

exit 0
