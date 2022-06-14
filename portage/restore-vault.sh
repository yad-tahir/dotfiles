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

./restore-common.sh

dst="/etc/portage/sets"
src="${PWD}/system${dst}"
sudo-do-sync "$src" "$dst"

dst="/etc/portage"
src="${PWD}/system${dst}/common.conf"
sudo-do-sync "$src" "$dst"

dst="/etc/portage/package.license"
src="${PWD}/system${dst}"
sudo-do-sync "$src" "$dst"

dst="/etc/portage/package.mask"
src="${PWD}/system${dst}/vault.mask"
sudo-do-sync "$src" "$dst"

dst="/etc/portage/package.accept_keywords"
src="${PWD}/system${dst}/vault"
sudo-do-sync "$src" "$dst"

arr=( "/etc/portage/package.use/apps"
	  "/etc/portage/package.use/core"
	  "/etc/portage/package.use/graphic"
	  "/etc/portage/package.use/misc"
	  "/etc/portage/package.use/networking"
	  "/etc/portage/package.use/server"
	  "/etc/portage/package.use/sound"
	  "/etc/portage/package.use/ui"
	  "/etc/portage/package.use/video"
	  "/etc/portage/package.use/vault"
	  "/etc/portage/package.use/vm"
	  "/etc/portage/package.use/zz-required"
	)

for i in "${arr[@]}"
do
	src=${PWD}/system${i}
	sudo-do-sync "$src" "/etc/portage/package.use"
done

dst="/etc/portage"
src="${PWD}/system/etc/portage/make-vault.conf"
sudo-do-sync "$src" "$dst" "make.conf"

dst="/var/lib/portage"
src="${PWD}/system${dst}/world-vault"
sudo-do-sync "$src" "$dst" "world"

dst="/var/lib/portage"
src="${PWD}/system${dst}/world_sets-vault"
sudo-do-sync "$src" "$dst" "world_sets"

REPO_PATH="/var/db/repos/private"
if [ ! -e $REPO_PATH ]; then
	echo "-> Download my private overlay"
	sudo -E git clone git@github.com:yad-tahir/personal-overlay.git "$REPO_PATH"
fi

REPO_PATH="/var/db/repos/public"
if [ ! -e $REPO_PATH ]; then
	echo "-> Download my public overlay"
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
