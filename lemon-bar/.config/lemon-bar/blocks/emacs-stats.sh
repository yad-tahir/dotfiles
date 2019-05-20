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

EMACS_FIFO=${PANEL_FIFO}-emacs
[ -e "$EMACS_FIFO" ] && rm "$EMACS_FIFO"
mkfifo "$EMACS_FIFO" -m600

# Keep the pipeline open
# Source: https://bbs.archlinux.org/viewtopic.php?id=168461
sid=$(sleep 99d > $EMACS_FIFO &
	  echo $!) &

if [ "$#" -eq 0 ]; then

	while read line; do

		buffer_count="${line#?}"
		
		if [ $buffer_count -gt 30 ]; then
			emacs="%{F$COLOR_INDICATOR3} ${buffer_count}%{F-}"
		else
			emacs=" ${buffer_count}"
		fi

		echo "Se${emacs}"

	done < $EMACS_FIFO

else
	while true; do
		echo "Se${emacs}"
		sleep $1
	done
fi

kill $sid
