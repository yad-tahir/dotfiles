#!/bin/sh
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
	state=$(pulsemixer --get-volume | awk '{print $1}')
	if [ $state -gt 100 ]; then
		printf "%s%-7.7s%s\n" "Sv%{F${COLOR_BACKGROUND}}%{B${COLOR_INDICATOR4}}" " ${state}" "%{B-}%{F-}"
	elif  [ $state -gt 50 ]; then
		printf "%s%-7.7s%s\n" "Sv%{F${COLOR_FOREGROUND}}%{B${COLOR_BACKGROUND}}" " ${state}" "%{B-}%{F-}"
	elif  [ $state -gt 0 ]; then
		printf "%s%-7.7s%s\n" "Sv%{F${COLOR_FOREGROUND}}%{B${COLOR_BACKGROUND}}" " ${state}" "%{B-}%{F-}"
	else
		printf "%s%-7.7s%s\n" "Sv%{F${COLOR_INDICATOR3}}%{B${COLOR_BACKGROUND}}" " ${state}" "%{B-}%{F-}"
	fi
	if [ "$#" -eq 0 ]; then
		break
	else
		sleep $1
	fi
done
