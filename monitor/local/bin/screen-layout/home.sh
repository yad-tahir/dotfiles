#!/bin/bash

# Copyright (C) 2020

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

# Import utility functions
cd `dirname $0`
. $PWD/util.sh

xrandr --output eDP-1 --off
xrandr --output DP-3  --mode 3840x2160 --pos 0x0 --rotate left --brightness 1
xrandr --output DP-1  --mode 3840x1600 --pos 2160x980 --rotate normal --primary --brightness 1

dispwin -d 1 ~/.config/icc-profiles/U3818DW#2-2018-10-20-2347.icc
dispwin -d 3 ~/.config/icc-profiles/U2718Q#3-2018-10-21-0034.icc

# Set desktop
util-reset-desktops DP-1
bspc monitor DP-1  -d 1 2 3 4 5 &> /dev/null
bspc monitor DP-3  -d 6 7 8 9 10 &> /dev/null
bspc monitor eDP-1 -r

# Reset padding
bspc config bottom_padding 0
bspc config right_padding 0

# Workspace 8 and 9 are use reading activities, it makes sense to use the
# monocle layout for portrait monitor
bspc desktop 8 -l monocle
bspc desktop 9 -l monocle

# Enable energy-saving modes
xset +dpms
xset s on

util-set-dpi 120
util-setup-services

# Startup apps
sleep 1 # small waiting time to ensure previous commands are done
urxvtc -hold -name log -e /bin/journalctl -fn 200 &
urxvtc -hold -name htop -e /usr/bin/htop &
