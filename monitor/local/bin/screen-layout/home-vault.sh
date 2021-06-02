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

xrandr --auto
xrandr --output DisplayPort-0 --mode 3840x2160 --pos 0x0 --rotate left
xrandr --output DisplayPort-1 --mode 3440x1440 --pos 2360x196 --rotate inverted
xrandr --output DisplayPort-2 --off
xrandr --output HDMI-A-0 --primary --mode 3840x1600 --pos 2160x1637 --rotate normal

# dispwin -d 1 $HOME/.config/icc-profiles/U3818DW#2-2018-10-20-2347.icc
# dispwin -d 3 $HOME/.config/icc-profiles/U2718Q#3-2018-10-21-0034.icc

# Enable energy-saving modes
xset +dpms
xset s on

util-set-dpi 110

# Startup apps
util-launch
sleep 0.5 # small waiting time to ensure previous commands are done
dwmc focusmon -1
dwmc focusmon -1
dwmc setnmasters 1
dwmc setmfact 0.5
urxvtc -hold -name log -e /bin/journalctl -fn 200 &
sleep 0.1
urxvtc -hold -name htop -e /usr/bin/htop &
sleep 0.1
dwmc focusmon 1

dwmc setnmasters 2
dwmc setmfact 0.66
