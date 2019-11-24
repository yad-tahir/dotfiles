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
xrandr --output DP3  --mode 1920x1200 --pos 0x0

# Set the monitors
bspc query -D -m DP1 | xargs -n 1 -I % bspc desktop % --to-monitor eDP1
bspc query -D -m DP3 | xargs -n 1 -I % bspc desktop % --to-monitor eDP1
bspc monitor eDP1 -d 1 2 3 4 5 6 7 8 9 10 &> /dev/null

xrandr --output eDP1 --dpi 250
xrandr --output DP1  --dpi 250
xrandr --output DP3  --dpi 250

# restart the bar
systemctl --user restart lemon-bar.service &

# Hide the lemon-bar to prevent OLED burning
sleep 0.1 && xdo id -a "$PANEL_WM_NAME" | xargs -n 1 -I % xdo hide %
bspc config top_padding 0

xsetroot -solid "$COLOR_BACKGROUND" &

# Fix resolution
bspc config right_padding 0
bspc config bottom_padding 0

# Stop turning off screens
xset -dpms
xset s off
