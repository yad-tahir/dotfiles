#!/bin/bash

# Copyright (C) 2020

# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 2 of the License, or (at your option) any later
# version.

# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
# Street, Fifth Floor, Boston, MA 02110-1301, USA.

# A small shell script called by BSPWM to automatically spawn window on the
# newest receptacle, preselection, or a new node launched. The aim here is to
# fill the receptacle or preselection first. Otherwise for some desktops, put
# nodes in a three-column layout, making it very suitable for ultra-wide
# monitors.

# This script is inspired the settings in https://gitlab.com/protesilaos/dotfiles

recept="$(bspc query -N -n 'any.leaf.!window')"
presel="$(bspc query -N -n 'newest.!automatic')"

# The priority goes to receptacle, presels and marked windows, respectively.
# Follow new window in case we are on a different desktop.
if [ -n "$recept" ]; then
	echo "node=$recept"
	echo "follow=on"
elif [ -n "$presel" ]; then
	echo "node=$presel"
	echo "follow=on"
else
	# The positional arguments are defined in the `external_rules_command` of bspc.
	window_id="$1"
	window_class="$2"
	window_instance="$3"
	# On Desktop change, we must stop executing the rest of the script as it
	# will confuse BSPWM.
	case "$window_class" in
		Steam)
			echo "follow=on"
			echo "state=tiled"
			exec echo "desktop=5"
			;;
		URxvt)
			case "$window_instance" in
				"pulsemixer")
					echo "follow=on"
					echo "state=floating"
					exec echo "sticky=on"
					;;
				"htop"|"glances"|"log")
					echo "follow=on"
					exec echo "desktop=10"
					;;
				"man")
					echo "follow=on"
					exec echo "desktop=9"
					;;
			esac
			;;
		Zathura)
			echo "follow=on"
			echo "state=tiled"
			exec echo "desktop=8"
			;;
	esac

	# Set nodes in the three-column layout if the current desktop is 1 or 2
	d=$(bspc query -D -d --names)
	if [ "$d" -le 2 ]; then

		# Check number of nodes
		# @TODO @FIX-ME This line can crash BSPWM if:
		# - We printed 'state=title' before running this line.
		# - We printed 'node=#' before running this line.
		n=$(bspc query -N -n .tiled.local | wc -l)

		# Target and replace the master node if it exists. In the three-column
		# layout, the master node is the node in the middle of the screen.
		first=$(bspc query -N -n any.tiled.local)
		master=$(bspc query -N -n ${first}\#east)

		if [ ! -z "$master" ]; then
			echo "node=$master"
		fi

		if [ $n == 1 ]; then
			# Push the current master to left so that we can put the new node
			# on the right side.
			# You need to be careful when you close the left-most node as BSPWM
			# may destroy the three-column layout - Check sxhkb's keybindings
			# for closing nodes.
			echo "split_dir=east"
			echo "split_ratio=0.33333333" # = 1/3
		elif [ $n == 2 ]; then
			# Push the current master to right so that we can put the new node
			# in the middle
			echo "split_dir=west"
			echo "split_ratio=0.5"
		fi
	fi
fi
