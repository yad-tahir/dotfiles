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

. $HOME/bin/settings.sh

while true; do
	ping=$(ping 8.8.8.8 -c 3 2> /dev/null |
			   awk '/avg/{split($4,arr,"/"); printf("%3.0f\n",arr[2])}' |
			   awk '$1=$1' 2> /dev/null)

	if [ $ping ]; then
		if [ $ping -gt 200 ]; then
			printf "%s%-5s%s\n" "Si%{F$COLOR_INDICATOR3} " "${ping}ms" "%{F-}"
		else
			printf "%s%-5s\n" "Si " "${ping}ms"
		fi
	else
			printf "%s%s\n" "Si%{F$COLOR_INDICATOR3}" "%{F-}"
	fi

	if [ "$#" -eq 0 ]; then
		break
	else
		sleep $1
	fi

done
