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
# newest receptacle or preselection. This script is inspired by:
# https://gitlab.com/protesilaos/dotfiles

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
fi


# @TODO Remove the code below if you don't need it
# The positional arguments are defined in the `external_rules_command` of bspc.
window_id="$1"
window_class="$2"
window_instance="$3"
