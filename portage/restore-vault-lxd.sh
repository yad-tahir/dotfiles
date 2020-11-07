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


TARGET="/etc/portage/repos.conf"
[ ! -e "${TARGET}" ] && sudo mkdir --parents "${TARGET}"

arr=( "/etc/portage/bashrc"
	  "/etc/portage/common-lxd.conf"
	  "/etc/portage/package.license"
	  "/etc/portage/package.use/core-lxd"
	  "/etc/portage/repos.conf/gentoo.conf"
	  "/etc/portage/repos.conf/public.conf"
	  "/etc/portage/package.mask"
	  "/etc/portage/package.accept_keywords"
	)

for i in "${arr[@]}"
do
	TARGET=$i
	SOURCE=${PWD}/system${TARGET}
	do-sync-sudo "$SOURCE" "$TARGET"
done

TARGET="/etc/portage/make.conf"
SOURCE="${PWD}/system/etc/portage/make-vault-lxd.conf"
do-sync-sudo "$SOURCE" "$TARGET"

REPO_PATH="/var/db/repos/public"
if [ ! -e $REPO_PATH ]; then
	echo "-> Download my public overlay"
	sudo -E git clone --depth=1 https://github.com/yad-tahir/gentoo-overlay.git "$REPO_PATH"
fi

REPO_PATH="/var/db/repos/gentoo"
if [ ! -e $REPO_PATH ]; then
	echo "-> Download Gentoo Github mirror"
	sudo -E git clone --depth=1 https://github.com/yad-tahir/gentoo-mirror.git "$REPO_PATH"
	pushd .
	cd $REPO_PATH
	sudo -E git checkout rsync
	popd
fi

do-ln-sync "$PWD/local/alias.d" "$HOME/.config/alias.d"
