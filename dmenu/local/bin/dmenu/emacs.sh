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

# A small menu to launch an Emacs command

# Get the settings
COLOR_BG=$(xrdb -query | awk '/\*background:/{print $2}')
COLOR_FG=$(xrdb -query | awk '/\*foreground:/{print $2}')
COLOR_SEL=$(xrdb -query | awk '/\*color2:/{print $2}')
FONT=$(xrdb -query | awk '/\Panel.font1:/{$1="";print $0}')
HEIGHT=$(xrdb -query | awk '/\Panel.height:/{print $2}')

emacs_commands="do-capture
				do-agenda
				do-music-playlist
				do-scratch-buffer
				do-calculator
				do-notebook
				do-file-manager"

for i in $emacs_commands; do
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
				-l 0 -p "Emacs" "$@")


if [ ! -z "${run}" -a "${run}" != "" ]; then
	exec emacsclient -qne "(${run})" > /dev/null
fi
