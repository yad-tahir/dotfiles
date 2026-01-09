#!/bin/bash
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

function do-uninstall {
	local src=$1 #source file or directory
	local dst=$2 #destination directory
	local rename=$3

	logger -p debug running do-uninstall src=$src dst=$dst rename=$rename uid=$UID

	[ ! -n "$src" ] && echo source cannot be empty && return -1

	# Directory mode?
	if [ -d "$src" ]; then
		logger -p debug running do-uninstall in the directory mode
		# Remove all the files existing in $src from $dst
		for f in $src/*; do
			[ -e "$dst/$(basename $f)" ] &&
				rm -R "$dst/$(basename $f)" &&
				logger -p info remove "$dst/$(basename $f)"
		done
	else
		logger -p debug running do-uninstall in the file mode
		if [ -n "$rename" ]; then
			name="$dst/$rename"
		else
			name="$dst/$(basename $src)"
		fi

		if	[ -f "$name" ]; then
				rm "$name" &&
				logger -p info remove "$name"
		fi
	fi

	# If $dst becomes empty, remove it as well
	[ -d "$dst" ] && [ -z "$(ls -A $dst)" ] &&
		rmdir "$dst" &&
		logger -p info remove the destination directory as well since it becomes empty
}

function sudo-do-uninstall {
	FUNC=$(declare -f do-uninstall)
	sudo bash -c "$FUNC; do-uninstall $1 $2 $3"
}

function do-install {
	local src=$1 #source file or directory
	local dst=$2 #destination directory
	local rename=$3 #new file name

	[ ! -e "$dst" ] && mkdir "$dst"
	[ ! -d "$dst" ] && echo destination must be a directory && return -1
	[ ! -n "$src" ] && echo source cannot be empty && return -1

	logger -p debug running do-install src=$src dst=$dst rename=$rename uid=$UID

	# Directory mode?
	if [ -d "$src" ]; then
		logger -p debug running do-install in the directory mode
		# Copy each file in $src to $dst
		( shopt -s dotglob
			for f in $src/*; do
				cp -R "$f" "$dst/$(basename $f)"
			done)
	else
		logger -p debug running do-install in the file mode
		logger -p info copy "$src" to  "$dst/$rename"
		cp "$src" "$dst/$rename"
	fi
}

function sudo-do-install {
	FUNC=$(declare -f do-install)
	sudo bash -c "$FUNC; do-install $1 $2 $3"
}

function do-sync {
	do-uninstall $1 $2 $3
	do-install $1 $2 $3
}

function sudo-do-sync {
	sudo-do-uninstall $1 $2 $3
	sudo-do-install $1 $2 $3
}
