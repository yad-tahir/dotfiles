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

arr=( "/etc/portage/bashrc"
	  "/etc/portage/make.conf"
	  "/etc/portage/repos.conf"
	  "/etc/portage/sets"
	  "/etc/portage/package.license"
	  "/etc/portage/package.use"
	  "/etc/portage/package.mask"
	  "/etc/portage/package.accept_keywords"
	)

for i in "${arr[@]}"
do
	TARGET=$i
	SOURCE=${PWD}/system${TARGET}
	do-sync-sudo "$SOURCE" "$TARGET"
done

arr=( "/var/lib/portage/world"
	  "/var/lib/portage/world_sets"
	)

for i in "${arr[@]}"
do
	TARGET=$i
	SOURCE=${PWD}/system${TARGET}
	do-ln-sync-sudo "$SOURCE" "$TARGET"
done

REPO_PATH="/var/db/repos/drvegeta"
if [ ! -e $REPO_PATH ]; then
	echo "-> Download DrVegeta portage overlay"
	sudo -E git clone git@github.com:yad-tahir/gentoo-overlay.git "$REPO_PATH"
fi

REPO_PATH="/var/db/repos/gentoo"
if [ ! -e $REPO_PATH ]; then
	echo "-> Download Gentoo Github mirror"
	sudo -E git clone git@github.com:yad-tahir/gentoo-mirror.git "$REPO_PATH"
	pushd .
	cd $REPO_PATH
	sudo -E git checkout without-manifest
	sudo -E git checkout rsync
	sudo -E git remote add gentoo https://anongit.gentoo.org/git/repo/gentoo.git
	popd
fi


do-ln-sync "$PWD/local/alias.d" "$HOME/.config/alias.d"
