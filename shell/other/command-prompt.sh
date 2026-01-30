#!/bin/sh

# Copyright (C) 2026

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

#
# Command prompt customization
#

# inspiration: https://www.youtube.com/watch?v=g4AfeQhRXrY&t=765s

prompt_timer_start() {
	timer=${timer:-$(date +%s%N)}
}

# Use the trap command to listen to 'DEBUG' events, which occur before executing
# the command
trap 'prompt_timer_start' DEBUG

prompt_generator() {

	# Calc the time different (in millisecond) between before and after command
	# execution.
	local delta=$(( ($(date +%s%N) - timer) / 1000000 ))

	# If it is less then a second
	if [ $delta -le 1000 ]; then
		# Use the escape character '\033[0' to make the text colorful. The
		# syntax goes as follows: \033[<font-style>;<color-number>m$ \033[0m
	    delta="\[\033[32m\]${delta}ms"
	else
		delta=$(( ${delta} / 1000 ))
		# If it is less then a minute
		if [[ $delta -le 60 ]]; then
			delta="\[\033[1;35m\]${delta}s"
		else
			# More than a minute
			local reminder;
			reminder=$(( ${delta} % 60 ))
			delta=$(( ${delta} / 60 ))
			delta="\[\033[1;33m\]${delta}:${reminder}m"
		fi
	fi

	# Check if it is root or not
	if [ $UID -eq 0 ]; then
		# \w to print current working directory
		PS1="${delta} \[\033[1;33m\]\w \\$"
	else
		PS1="${delta} \[\033[1;36m\]\w \[\033[34m\]\\$"
	fi

	# Reset the font color back to default
	PS1="${PS1}\[\033[0m\] "

	unset timer
}

PROMPT_COMMAND=prompt_generator
