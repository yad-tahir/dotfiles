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



tmp=$(insync-headless get_sync_progress 2> /dev/null | head -n1)
status=$(insync-headless get_status 2> /dev/null)

if [ "$tmp" == "No syncing activities" ] && [ "$status" == "SHARE" ]; then
	echo ""
elif [ "$tmp" == "Download" ]; then
	COLOR=$(xrdb -query | awk '/\*color2:/{print $2}')
	echo "%{F$COLOR} Downloading%{F-}"
elif [ "$tmp" == "Uploading" ]; then
	COLOR=$(xrdb -query | awk '/\*color9:/{print $2}')
	echo "%{F$COLOR} Uploading%{F-}"
else
	COLOR=$(xrdb -query | awk '/\*color11:/{print $2}')
	echo "%{F$COLOR} $status%{F-}"
fi