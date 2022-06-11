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

xrandr --output DisplayPort-2 --off
xrandr --output HDMI-A-0 --primary --mode 3840x1600 --pos 2160x1637 --rotate normal
sleep 1
xrandr --output DisplayPort-0 --mode 3840x2160 --pos 0x0 --rotate right
sleep 1
xrandr --output DisplayPort-1 --mode 3440x1440 --pos 2360x196 --rotate inverted
sleep 1

sleep 60 && lxc exec displaycal -- sudo -u lxd argyll-dispwin -d 1 '/home/lxd/.local/share/DisplayCAL/storage/U3818DW-2022-01-23/2022-01-23-100.icc' &

sleep 70 && lxc exec displaycal -- sudo -u lxd argyll-dispwin -d 2 '/home/lxd/.local/share/DisplayCAL/storage/U2718Q-2022-01-23-8/2022-01-23-100.icc' &

sleep 80 && lxc exec displaycal -- sudo -u lxd argyll-dispwin -d 3 '/home/lxd/.local/share/DisplayCAL/storage/W3415W-2022-01-23/2022-01-23-100.icc' &

# Enable energy-saving modes
xset +dpms
xset s on

util-set-dpi 110

# Startup apps
util-launch && sleep 0.1

dwmc setnmasters 2
dwmc setmfact 0.66
dwmc focusmon -1 && sleep 0.1
dwmc setnmasters 2
dwmc setmfact 0.66
dwmc focusmon -1 && sleep 0.1
dwmc setnmasters 2
dwmc setmfact 0.66


dwmc focusmon 1 && sleep 0.1
exec urxvtc -hold -name log -e /bin/journalctl -fn 200
