#!/bin/bash

xrandr --output HDMI-0 --off --output DP-4 --mode 3440x1440 --pos 2880x0 --rotate normal --output DP-3 --mode 3440x1440 --pos 6320x0 --rotate normal --output DP-2 --mode 2880x1800 --pos 0x0 --rotate normal --output DP-1 --off --output DP-0 --off

# Set the icc profile of each screen
dispwin -d 2 ~/.config/icc-profiles/U3415W#2-2018-10-21-1137.icc
dispwin -d 3 ~/.config/icc-profiles/U3415W#3-2018-10-21-1221.icc

# Set the monitors
bspc monitor DP-2 -n DP-2 -d 10 &> /dev/null
bspc monitor DP-3 -n DP-3 -d 1 2 3 4 5 &> /dev/null
bspc monitor DP-4 -n DP-4 -d 6 7 8 9 &> /dev/null

# restart the bar
systemctl --user restart lemon-bar.service &


xsetroot -solid "$COLOR_BACKGROUND" &
