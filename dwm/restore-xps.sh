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

cd `dirname $0`
echo "* Execute ${PWD}/`basename $0`"
. ../utils.sh

# System-wide
arr=( "/etc/NetworkManager/dispatcher.d"
	  "/etc/udev/rules.d"
	  "/usr/bin" )

for i in "${arr[@]}"
do
	dst=$i
	src=${PWD}/system${dst}
	sudo-do-sync "$src" "$dst"
done

# Local
do-sync "${PWD}/local/config/sxhkd" "${HOME}/.config/sxhkd"

dst="${HOME}/.config/dwm"
[ ! -e "${dst}" ] && mkdir --parents "${dst}"
do-sync "${PWD}/local/config/dwm/dwmrc-xps" "${dst}" "dwmrc"
do-sync "${PWD}/local/config/dwm/dwmbar-xps" "${dst}" "dwmbar"

dst="${HOME}/.config/systemd/user"
do-sync "${PWD}/local/systemd/user" "${dst}"
