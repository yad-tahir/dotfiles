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

function util-set-dpi {
	local dpi=$1
	xrdb -merge <<EOF
	Xft.dpi: $dpi
EOF
	xrandr --dpi $dpi
}

function util-setup-services {
	# Restart apps that depend on environmental variables
	feh --bg-fill $(ls ${HOME}/pictures/background/* | shuf -n 1) &

	systemctl --user restart emacs-27-vcs.service&
	systemctl --user restart urxvtd.service &
}

function util-launch-polybar-all {
	killall polybar 2> /dev/null
	bspc query -M --names | xargs -I % -n 1 sh -c 'MONITOR=% polybar orange &' &
}

function util-reset-desktops {
	local primary=$1

	# Reset desktop layouts
	bspc query -D | xargs -n 1 -I % bspc desktop % -l tiled

	# Move all the existing nodes to the primary monitor
	bspc query -D -m eDP-1 | xargs -n 1 -I % bspc desktop % --to-monitor $primary
	bspc query -D -m DP-1 | xargs -n 1 -I % bspc desktop % --to-monitor $primary
	bspc query -D -m DP-3 | xargs -n 1 -I % bspc desktop % --to-monitor $primary

	# Move tiled nodes to one desktop. This is needed because sometimes
	# BSPWM bugs out when we switch between various monitors.
	bspc query -N -n .tiled | xargs -n 1 -I % bspc node % -d 1
}
