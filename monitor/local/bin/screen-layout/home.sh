#!/bin/sh

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

xrandr --output eDP-1 --off
xrandr --output DP-3  --mode 3840x2160 --pos 0x0 --rotate left --brightness 1
xrandr --output DP-1  --mode 3840x1600 --pos 2160x980 --rotate normal --primary --brightness 1

dispwin -d 1 ~/.config/icc-profiles/U3818DW#2-2018-10-20-2347.icc
dispwin -d 3 ~/.config/icc-profiles/U2718Q#3-2018-10-21-0034.icc

# Set the workspaces
# Move current tiled nodes to a desktop. This is needed because sometimes
# BSPWM bugs out when we switch between various monitors.
bspc query -N -n .tiled | xargs -n 1 -I % bspc node % -d 1
bspc query -D -m eDP-1 | xargs -n 1 -I % bspc desktop % --to-monitor DP-1
bspc query -D -m DP-3 | xargs -n 1 -I % bspc desktop % --to-monitor DP-1
bspc monitor DP-1  -d 1 2 3 4 5 &> /dev/null
bspc monitor DP-3  -d 6 7 8 9 10 &> /dev/null
bspc monitor eDP-1 -r

# Reset desktop layouts
bspc query -D | xargs -n 1 -I % bspc desktop % -l tiled

# Workspace 8 and 9 are use reading activities, it makes sense to use the
# monocle layout for portrait monitor
bspc desktop 8 -l monocle
bspc desktop 9 -l monocle

xrandr --output DP-1  --dpi 280
xrandr --output DP-3  --dpi 280

# Refresh the background
feh --bg-fill $(ls ${HOME}/pictures/background/* | shuf -n 1) &

# Launch Apps
killall polybar
bspc query -M --names | xargs -I % -n 1 sh -c 'MONITOR=% polybar orange &'

systemctl --user start urxvtd.service &
sleep 1
urxvtc -hold -name log -e /bin/journalctl -fn 200 &
urxvtc -hold -name htop -e /usr/bin/htop &

# bspc desktop 6 -f &&
# urxvtc -e /bin/sh -c 'htop;bash' &&
# urxvtc -e /bin/sh -c 'journalctl -fn 100;bash' &
