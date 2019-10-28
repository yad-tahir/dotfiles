#!/bin/sh

# Copyright (C) 2019

# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301, USA.

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

sudo ip route add 172.30.65.0/24 via 172.17.2.1 dev wlp4s0
