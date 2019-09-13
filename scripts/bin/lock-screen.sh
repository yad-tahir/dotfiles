#!/bin/sh

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


# Take a screen shot
file=/tmp/screen.png

# Remove the file if it exists
rm "${file}" 2> /dev/null

# Take a screenshot
scrot "${file}"

# Rescale it to make it blurry
convert "${file}" -scale 15% -scale 667.5% "${file}"

# Lock the screen and put the modified image in background
i3lock -u -i "${file}" &
