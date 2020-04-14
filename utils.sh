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

function do-rm-sudo {
	local SOURCE=$1
	local TARGET=$2
	if [ -d "$SOURCE" ]; then
		/bin/ls "$SOURCE" | xargs -I % -n 1 sudo rm "${TARGET}/%"
	else
		sudo rm "${TARGET}"
	fi
}

function do-rm {
	local SOURCE=$1
	local TARGET=$2
	if [ -d "$SOURCE" ]; then
		/bin/ls "$SOURCE" | xargs -I % -n 1 rm "${TARGET}/%"
	else
		rm "${TARGET}"
	fi
}

function do-sync-sudo {
	local SOURCE=$1
	local TARGET=$2
	do-rm-sudo "$SOURCE" "${TARGET}"
	if [ -d "$SOURCE" ]; then
		[ ! -e "${TARGET}" ] && sudo mkdir "${TARGET}"
		/bin/ls "$SOURCE" | xargs -I % -n 1 sudo cp "${SOURCE}/%" "${TARGET}/%"
	else
		sudo cp "${SOURCE}" "${TARGET}"
	fi
}

function do-sync {
	local SOURCE=$1
	local TARGET=$2
	do-rm "$SOURCE" "${TARGET}"
	if [ -d "$SOURCE" ]; then
		[ ! -e "${TARGET}" ] && mkdir "${TARGET}"
		/bin/ls "$SOURCE" | xargs -I % -n 1 cp "${SOURCE}/%" "${TARGET}/%"
	else
		cp "${SOURCE}" "${TARGET}"
	fi
}

function do-ln-sync {
	local SOURCE=$1
	local TARGET=$2
	do-rm "$SOURCE" "${TARGET}"
	if [ -d "$SOURCE" ]; then
		[ ! -e "${TARGET}" ] && mkdir "${TARGET}"
		/bin/ls "$SOURCE" | xargs -I % -n 1 ln -s "${SOURCE}/%" "${TARGET}/%"
	else
		ln -s "${SOURCE}" "${TARGET}"
	fi
}

function do-ln-sync-sudo {
	local SOURCE=$1
	local TARGET=$2
	do-rm-sudo "$SOURCE" "${TARGET}"
	if [ -d "$SOURCE" ]; then
		[ ! -e "${TARGET}" ] && sudo mkdir "${TARGET}"
		/bin/ls "$SOURCE" | xargs -I % -n 1 sudo ln -s "${SOURCE}/%" "${TARGET}/%"
	else
		sudo ln -s "${SOURCE}" "${TARGET}"
	fi
}
