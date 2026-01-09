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

function util-launch {
	# Restart apps that depend on environmental variables
	feh --bg-fill --no-fehbg -Nq $(/bin/ls ${HOME}/pictures/background/* | shuf -n 1) &

	# Start services
	systemctl --user restart emacs.service &
	systemctl --user restart urxvtd.service &
}
