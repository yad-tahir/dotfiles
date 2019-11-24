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

if [ "$#" -eq 0 ]; then
	NVIDIA_FIFO=${PANEL_FIFO}-nvidia
	[ -e "$NVIDIA_FIFO" ] && rm "$NVIDIA_FIFO"
	mkfifo "$NVIDIA_FIFO" -m777
	# Keep the pipline open for 99 days; Otherwise, kernel will close it after the first read
	sleep 99d > "$NVIDIA_FIFO" &

	while read line ; do
		if [ $line = 'module-loaded' ]; then
			echo "Sz%{F$COLOR_INDICATOR1} %{F-}"
		else
			echo "Sz"
		fi
	done < $NVIDIA_FIFO
else
	while true; do
	CODE=$(cat /sys/bus/pci/devices/0000\:01\:00.0/power/runtime_status)

	if [ $CODE = "active" ]; then
		echo "Sz%{F$COLOR_INDICATOR1} %{F-}"
	else
		echo "Sz"
	fi
		sleep $1
	done
fi
