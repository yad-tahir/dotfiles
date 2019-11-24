#! /bin/bash
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

function print_num_bugs {
	c=$(journalctl -b -p err --no-pager --no-tail --no-full | wc -l)
	if [ $c -gt 30 ]; then
		c="%{F$COLOR_INDICATOR4}"$c"%{F-}"
	else
		c=" "$c
	fi
	echo "Sb" $c
}

if [ "$#" -eq 0 ]; then
	BUGS_FIFO=${PANEL_FIFO}-bugs
	[ -e "$BUGS_FIFO" ] && rm "$BUGS_FIFO"
	mkfifo "$BUGS_FIFO" -m600
	journalctl -f -b -p err --no-pager --no-tail --no-full > $BUGS_FIFO &

	while read line ; do
		print_num_bugs
	done < $BUGS_FIFO
else
	while true; do
		print_num_bugs
		sleep $1
	done
fi
