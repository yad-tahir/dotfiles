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

xrandr --output HDMI-0 --off --output DP-4 --mode 3840x2160 --pos 2880x0 --rotate left --output DP-3 --mode 3840x1600 --pos 5040x0 --rotate normal --output DP-2 --mode 2880x1800 --pos 0x0 --rotate normal --output DP-1 --off --output DP-0 --off

dispwin -d 2 ~/.config/icc-profiles/U3818DW#2-2018-10-20-2347.icc
dispwin -d 3 ~/.config/icc-profiles/U2718Q#3-2018-10-21-0034.icc

# Set the workspaces
bspc monitor DP-2 -n DP-2 -d 10 &> /dev/null
bspc monitor DP-3 -n DP-3 -d 1 2 3 4 5 &> /dev/null
bspc monitor DP-4 -n DP-4 -d 6 7 8 9 &> /dev/null

# restart the bar
systemctl --user restart lemon-bar.service &

# Refresh the background
xsetroot -solid "$COLOR_BACKGROUND" &

# Setup the sound system
systemctl --user restart pulseaudio.service &&
	pactl set-default-sink alsa_output.pci-0000_01_00.1.hdmi-stereo-extra2 &&
	pactl set-card-profile 0 output:hdmi-stereo-extra2 &

# Launch Apps
# bspc desktop 6 -f &&
# urxvtc -e /bin/sh -c 'htop;bash' &&
# urxvtc -e /bin/sh -c 'journalctl -fn 100;bash' &
