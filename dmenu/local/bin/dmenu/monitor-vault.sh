#!/bin/sh

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

# A small menu to choose the monitor setup

# Get the settings
source "`dirname $0`/dmenu-util"
COLOR_SEL="#c0ff3e"

layouts="home"
for i in $layouts; do
	echo_string="${i}\\n${echo_string}"
done

run=$(echo -e "${echo_string}" |
		  head -n-1 |
		  dmenu -i -f\
				-nb "$COLOR_BG" \
				-nf "$COLOR_FG" \
				-sb "$COLOR_SEL" \
				-sf "$COLOR_BG" \
				-fn "${FONT}" \
				-h "${HEIGHT}" \
				-l 0 -p "Monitor" "$@")


if [ ! -z "${run}" -a "${run}" != "" ]; then
	exec $HOME/bin/screen-layout/"${run}.sh" > /dev/null
fi
