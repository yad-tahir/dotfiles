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

# A utility script to draw a progress using text underline.

. $HOME/bin/settings.sh

text="$1"
progress="$2"

if [ $progress -eq $progress 2> /dev/null ]; then
	len="${#text}"
	# Calculate the number of characters that needs to be underline
	underline_num=$(($len*progress/100))

	# Make sure $underline_num never exceeds the length
	if [ "$underline_num" -gt $len ]; then
		underline_num=$len
	fi

	output=$(echo "$text" |
				 awk -v UNDER="$underline_num" \
					 '{print "%{+u}"substr($0,0,UNDER) \
				"%{-u}"substr($0,UNDER+1,length($0))}')

	echo "$output"
fi
