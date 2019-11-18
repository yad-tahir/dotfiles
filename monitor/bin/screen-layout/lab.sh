#! /bin/sh

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

xrandr --output eDP1 --mode 2560x1440 --pos 0x0
xrandr --output DP1  --mode 1920x1200 --pos 0x0

#Fix DP-2 resolution. A small hack to make text more readable with AUIS's projecter
# xrandr  --output DP1 --scale-from 2048x1024

# Set the monitors
bspc monitor DP1 -d 1 2 3 4 5 6 7 8 9 10 &> /dev/null

# restart the bar
systemctl --user restart lemon-bar.service &

xsetroot -solid "$COLOR_BACKGROUND" &

# Fix resolution
bspc config right_padding 0
bspc config bottom_padding 0

# Stop turning off screens
xset -dpms
xset s off
