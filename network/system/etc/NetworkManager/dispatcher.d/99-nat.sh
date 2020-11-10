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

case "$1" in
	wlp*)
		case "$2" in
			up)
				ip address add 192.168.1.125 dev "$1" 2> /dev/null
				ip address add 192.168.1.126 dev "$1" 2> /dev/null
				ip address add 192.168.1.127 dev "$1" 2> /dev/null
				ip address add 192.168.1.128 dev "$1" 2> /dev/null
				ip address add 192.168.1.129 dev "$1" 2> /dev/null
				true;
				;;
			down)
				;;
			connectivity-change)
				;;
		esac
		;;
esac
