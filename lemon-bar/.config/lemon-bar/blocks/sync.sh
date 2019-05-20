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

while true; do
	tmp=$(insync-headless get_syn_progress | head -n1)

	if [ "$tmp" != "No syning activities" ]; then
		echo "Sg"
	elif [ "$tmp" = "Download" ]; then
		echo "Sg%{F$COLOR_INDICATOR1} %{F-}"
	elif [ "$tmp" = "Uploading" ]; then
		echo "Sg%{F$COLOR_INDICATOR3} %{F-}"
	else
		echo "Sg%{F$COLOR_INDICATOR2} %{F-}"
	fi

	if [ "$#" -eq 0 ]; then
		break
	else
		sleep $1
	fi
done
