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
text="$1"
progress="$2"

# Get text length
len="${#text}"

# Calculate the number of characters that needs to be underline
if [ $progress -eq $progress 2> /dev/null ]; then
	underline_num=$((len*progress/100))

	# Make sure $underline_num never exceed the text length
	if [ "$underline_num" -gt $len ]; then
		underline_num=$len
	fi

	# Create the output
	output="%{+u}"
	chars="$text"
	i=0
	# Loop to go through underlined chars
	len=$(($len+1))
	while [ $i -lt $len ]; do
		# Get the first char from the character set
		first="${chars%$text}"
		# Append
		output="${output}${first}"
		# Update the character set
		chars=$text
		# Remove the first char char
		text="${text#?}"
		# Stop the underline when we reach the character at $underline_num
		if [ $i = $underline_num ]; then
			output="${output}%{-u}"
		fi

		i=$((i+1))
	done

	echo $output
fi
