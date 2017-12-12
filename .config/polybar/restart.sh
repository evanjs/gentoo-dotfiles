#!/usr/bin/env bash

pkill polybar
polybar --config=$HOME/.config/polybar/config barr &
#polybar --config=$HOME/.config/polybar/config bar2 &

exit 0
