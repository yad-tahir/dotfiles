#!/bin/bash

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

# A small script to prepare a list of commands available in the system,
# then use dmenu to show it.

# Get the settings
source "`dirname $0`/dmenu-util"
COLOR_SEL="#ffa500"

run=$(
	echo_string="${echo_string}"$(IFS=: ; stest -flx $PATH |
									  sort -u |
									  uniq | head -n-2)

	echo -e "${echo_string}" | dmenu -i -f\
									-nb "$COLOR_BG" \
									-nf "$COLOR_FG" \
									-sb "$COLOR_SEL" \
									-sf "$COLOR_BG" \
									-fn "${FONT}" \
									-h "${HEIGHT}" \
									-l 0 -p "App" "$@"
   )

#Run the selected command
case "$run" in
	# If it ends with '!', run it in a terminal instead
	*!)
		exec $(echo "$TERMINAL -hold -e $run" | sed -e 's/!$//') &
		;;
	*?*)
		# Run the local version first, if it is failed then switch to the system one.
		exec "$run" &
		;;
esac
