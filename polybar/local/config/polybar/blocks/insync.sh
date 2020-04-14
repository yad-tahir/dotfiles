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



tmp=$(insync-headless get_sync_progress 2> /dev/null | head -n1)
status=$(insync-headless get_status 2> /dev/null)

if [ "$tmp" == "No syncing activities" ] && [ "$status" == "SHARE" ]; then
	echo "| "
else
	# Calculate the mean of the progress' percentage. Insync shares this data
	# when uploading or downloading files.
	per=$(insync-headless get_sync_progress |
			  awk '/[:number:]?%/{
						p=$2;
						gsub("\\(","",p);
						gsub("%","",p);
						# update sum and count
						s+=p
						n+=1;
					}
					END{
						# print the mean
						if (n != 0)
							printf ("%d%\n", s/n)
					}' 2> /dev/null)
	if [ "$tmp" == "Download" ]; then
		COLOR=$(xrdb -query | awk '/\*color2:/{print $2}')
		echo "| %{F$COLOR} Downloading $per%{F-}"
	elif [ "$tmp" == "Uploading" ]; then
		COLOR=$(xrdb -query | awk '/\*color9:/{print $2}')
		echo "| %{F$COLOR} Uploading $per%{F-}"
	else
		# For anything else, print it in the alert color.
		COLOR=$(xrdb -query | awk '/\*color11:/{print $2}')
		echo "| %{F$COLOR} $status%{F-}"
	fi
fi
