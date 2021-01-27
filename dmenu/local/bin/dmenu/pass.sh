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

# Get the settings
source "`dirname $0`/dmenu-util"
COLOR_SEL="#ff6347"

prefix="${PASSWORD_STORE_DIR}/"
# Get the files
password_files=$(find $prefix -name '*.gpg'|
					 awk '{gsub("'$prefix'","",$0);\
					 gsub("'.gpg'","",$0);\
					 print $0}')

value=$(printf '%s\n' "$password_files" |
			dmenu -i -f\
				  -nb "$COLOR_BG" \
				  -nf "$COLOR_FG" \
				  -sb "$COLOR_SEL" \
				  -sf "$COLOR_BG" \
				  -fn "${FONT}" \
				  -h "${HEIGHT}" \
				  -l 0 -p "Password" "$@")

if [ "$value" != "" ]; then
	. $HOME/bin/settings.sh
	pass show -c $value > /dev/null
fi
