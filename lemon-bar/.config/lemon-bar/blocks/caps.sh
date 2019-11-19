#! /bin/sh
#

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

while true; do
	state=$(xset -q | awk '/Caps/ {print $4}')
	if [ $state = "off" ]; then
		echo "Sy%{F${COLOR_INDICATOR6}}CAPS%{F-}"
	else
		echo "Sy%{F${COLOR_INDICATOR1}}CAPS%{F-}"
	fi
	if [ "$#" -eq 0 ]; then
		break
	else
		sleep $1
	fi
done
