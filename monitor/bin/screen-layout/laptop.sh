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

xrandr --output eDP-1 --auto --primary --brightness 1
xrandr --output DP-1 --off
xrandr --output DP-3 --off

# Set the monitors
bspc query -D -m DP-1 | xargs -n 1 -I % bspc desktop % --to-monitor eDP-1
bspc query -D -m DP-3 | xargs -n 1 -I % bspc desktop % --to-monitor eDP-1

bspc monitor DP-1 -r
bspc monitor DP-3 -r
bspc monitor eDP-1 -d 1 2 3 4 5 6 7 8 9 10 &> /dev/null

xrandr --output eDP-1 --dpi 300

# restart the bar
systemctl --user restart lemon-bar.service

# Hide the lemon-bar to prevent OLED burning
sleep 0.1 && xdo id -a "$PANEL_WM_NAME" | xargs -n 1 -I % xdo hide %
bspc config top_padding 0

sleep 1 && feh --bg-fill $(ls ${HOME}/pictures/background/* | shuf -n 1) &

# Stop turning off screens
# xset -dpms
# xset s off
