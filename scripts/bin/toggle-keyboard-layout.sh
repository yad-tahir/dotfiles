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

. $HOME/bin/settings.sh

state=$(setxkbmap -query |
				awk '/^layout/{split($2,arr,","); print toupper(arr[1])}')

if [ $state = "DVORAK" ]; then
	setxkbmap us
	setxkbmap -option
else
	setxkbmap dvorak
	# setxkbmap -option altwin:swap_alt_win
fi

# Restart refresh system-wide keybindings. This is to accommodate the changes in key codes
systemctl restart --user sxhkd.service

~/.config/lemon-bar/blocks/keyboard.sh > $PANEL_FIFO
