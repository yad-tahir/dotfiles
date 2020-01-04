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
COLOR_BG=$(xrdb -query | awk '/\*background:/{print $2}')
COLOR_FG=$(xrdb -query | awk '/\*foreground:/{print $2}')
COLOR_MAIN=$(xrdb -query | awk '/\*color10:/{print $2}')
FONT=$(xrdb -query | awk '/\Panel.font1:/{$1="";print $0}')

layouts="laptop lab office home"
for i in $layouts; do
	echo_string="${i}\\n${echo_string}"
done

run=$(echo -e "${echo_string}" |
		  dmenu -i -f\
				-nb "$COLOR_BG" \
				-nf "$COLOR_FG" \
				-sb "$COLOR_MAIN" \
				-sf "$COLOR_BG" \
				-fn "${FONT}" \
				-l 0 -p "Monitor" "$@")


if [ ! -z "${run}" -a "${run}" != "" ]; then
	exec ~/bin/screen-layout/"${run}.sh" > /dev/null
fi
