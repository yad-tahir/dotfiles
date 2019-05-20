#! /bin/sh

# Set the monitors
bspc monitor DP-2 -n DP-2 -d 1 2 3 4 5 6 7 8 9 10 &> /dev/null

# restart the bar
~/.config/lemon-bar/panel.sh &

xsetroot -solid "$COLOR_BACKGROUND" &
