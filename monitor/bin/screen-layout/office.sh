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

xrandr --output eDP-1 --off
xrandr --output DP-1	 --mode 3440x1440 --pos 3840x0 --rotate normal --brightness 1
xrandr --output DP-3	 --mode 3440x1440 --pos 7280x0 --rotate normal --primary --brightness 1

# Set the icc profile of each screen
dispwin -d 1 ~/.config/icc-profiles/U3415W#2-2018-10-21-1137.icc
dispwin -d 3 ~/.config/icc-profiles/U3415W#3-2018-10-21-1221.icc

# Set the monitors
bspc query -D -m eDP-1 | xargs -n 1 -I % bspc desktop % --to-monitor DP-1
bspc query -D -m DP-3 | xargs -n 1 -I % bspc desktop % --to-monitor DP-1
bspc monitor eDP-1 -r
bspc monitor DP-3  -d 1 2 3 4 5 &> /dev/null
bspc monitor DP-1  -d 6 7 8 9 &> /dev/null

xrandr --output DP-1  --dpi 200
xrandr --output DP-3  --dpi 200

# restart the bar
systemctl --user restart lemon-bar.service &

xsetroot -solid "$COLOR_BACKGROUND" &
