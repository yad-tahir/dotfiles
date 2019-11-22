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

while true; do
	ACPI=$(acpi)
	CODE=$(echo $ACPI | awk '{print tolower(substr($3,1,length($3)-1))}')
	BAT=$(echo $ACPI |  awk '{gsub(",","",$0);gsub("%","",$0);print $4}')
	if [ $CODE = "discharging" ]
	then
		if [ $BAT -ge 85 ];then
			F=$COLOR_INDICATOR2
		elif [ $BAT -ge 50 ]; then
			F=$COLOR_INDICATOR1
		else
			F=$COLOR_INDICATOR4
		fi
		echo "Sx%{F$F} ${BAT}%%{F-}"
	else
		echo "Sx ${BAT}%"
	fi

	if [ "$#" -eq 0 ]; then
		break
	else
		sleep $1
	fi
done
