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

xrandr --output eDP-1 --mode 2560x1440 --pos 0x0 --brightness 0.5
xrandr --output DP-1  --mode 1920x1200 --pos 0x0 --brightness 1
xrandr --output DP-3  --mode 1920x1200 --pos 0x0 --brightness 1

# Adjust DPI
util-set-dpi 120

# Set desktops
if [ $(bspc config bottom_padding 0 2> /dev/null) ]; then
	util-reset-desktops eDP-1
	bspc monitor eDP-1 -d 1 2 3 4 5 6 7 8 9 10 &> /dev/null

	# Fix resolution
	bspc config right_padding 640
	bspc config bottom_padding 210

	# Launch polybar to eDP-1 only since we are using xrandr's mirror mode
	killall polybar 2> /dev/null
	MONITOR=eDP-1 polybar orange &
fi

# Stop turning off screens when idle
xset -dpms
xset s off

# Restart apps that depend on environmental variables
util-setup-services
