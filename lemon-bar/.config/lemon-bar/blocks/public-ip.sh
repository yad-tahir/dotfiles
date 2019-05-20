#!/bin/sh
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

while true; do
	code=$(curl ipinfo.io 2> /dev/null | awk '/country/ {print substr($2,2,2)}' 2> /dev/null)
	if [ "$code" = "GB" ]; then
		code=" ${code}"
	elif [ "$code" = "" ]; then
		code="%{F${COLOR_INDICATOR3}}%{F-}"
	else
		code="%{F${COLOR_INDICATOR3}} ${code}%{F-}"
	fi

	echo "Sn${code}"

	if [ "$#" -eq 0 ]; then
		break
	else
		sleep $1
	fi
done
