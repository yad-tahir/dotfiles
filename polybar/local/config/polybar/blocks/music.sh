#! /bin/sh
#

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

current=0
len=20

state=$(mpc current -f '%title% %artist% %album%' 2> /dev/null)
pause=$(mpc status | awk '/paused/{print $0}' 2> /dev/null)

if [ "$state" = "" -o "$pause" != "" ]; then
	polybar-msg hook music 1 &> /dev/null
else
	position=$(mpc status | awk '/playing/{print $3}' 2> /dev/null)
	title=" $position $state"

	percent=$(mpc status | awk '/playing/{print substr($4,2,length($4)-3)}' 2> /dev/null)
	text=$($HOME/.config/polybar/blocks/progress.sh " $title " $percent)

	# print the formatted text
	echo "${text}"
	# Ask polybar to refresh the music module which in turn is going to call
	# this script again
fi
