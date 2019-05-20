#! /bin/bash

xrandr --output DP-4 --mode 1280x1024 --pos 0x0
xrandr --output DP-3 --mode 1280x1024 --pos 0x0

#Fix DP-2 resolution. A small hack to make text more readable with AUIS's projecter
xrandr  --output DP-2 --scale-from 2048x1024

# Set the monitors
bspc monitor DP-2 -n DP-2 -d 1 2 3 4 5 6 7 8 9 10 &> /dev/null

# restart the bar
systemctl --user restart lemon-bar.service &

xsetroot -solid "$COLOR_BACKGROUND" &
