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

# A small script to prepare a list of commands available in the system,
# then use dmenu to show it.

# Get the settings
COLOR_BG=$(xrdb -query | awk '/\*background:/{print $2}')
COLOR_FG=$(xrdb -query | awk '/\*foreground:/{print $2}')
COLOR_MAIN=$(xrdb -query | awk '/\*color9:/{print $2}')
FONT=$(xrdb -query | awk '/\Panel.font1:/{$1="";print $0}')


run=$(
		IFS='|'
		# Add custom commends to the demnu
		prefix_commands="steam-local|emacs|firefox-no-vpn-local|firefox-vpn"
		echo_string=
		filter_string=

		for i in $prefix_commands; do
			echo_string="${i}\\n${echo_string}"
			filter_string="${i}\$|${filter_string}"
		done
		# Delete the last character
		filter_string=$(awk -v VAL="$filter_string" \
						'BEGIN {print substr(VAL,0,length(VAL)-1)}')
		IFS=:
		{ echo -e "${echo_string}";
		  stest -flx $PATH |
			  awk "!/${filter_string}/" |
			  sort -u | uniq ; } |
			dmenu -i -f\
				  -nb "$COLOR_BG" \
				  -nf "$COLOR_FG" \
				  -sb "$COLOR_MAIN" \
				  -sf "$COLOR_BG" \
				  -fn "${FONT}" \
				  -l 0 -p "App" "$@"
   )



#Run the selected command
case "$run" in
	# If it ends with '!', run it in a terminal instead
	*!)
		exec $(echo "$TERMINAL -hold -e $run" | sed -e 's/!$//') &
		;;
	*local)
		exec $(echo "$HOME/bin/$run" | sed 's/-local//') &
		;;
	*?*)
		# Run the local version first, if it is failed then switch to the system one.
		exec "$run" &
		;;
esac
