#! /bin/bash

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

xrandr --output eDP-1 --auto --primary --brightness 0.5
xrandr --output DP-1 --off
xrandr --output DP-3 --off

# Set desktops
util-reset-desktops eDP-1
bspc monitor eDP-1 -d 1 2 3 4 5 6 7 8 9 10 &> /dev/null
bspc monitor DP-1 -r
bspc monitor DP-3 -r

util-set-dpi 140
util-setup-services
