#! /bin/sh
#

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



warning=$(xrdb -query | awk '/\*color9:/{print $2}')
error=$(xrdb -query | awk '/\*color11:/{print $2}')

# Get the country code for the public IP. Highlight it if it is not US.
curl ipinfo.io 2>&1 |
	awk '/country/{
		code=substr($2,2,2);
		if (code == "US"){
			print " " code
		}else{
			print "%{F'$warning'} " code "%{F-}"
		}
	}
	# On error
	/^curl/{
		print "%{F'$error'} NO-NET%{F-}"
	}'
