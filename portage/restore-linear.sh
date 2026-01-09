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

cd `dirname $0`
echo "* Execute ${PWD}/`basename $0`"
. ../utils.sh

cd "${PWD}/system/linear"
sudo rm -R "/etc/portage"
sudo-do-sync "${PWD}/etc" "/etc/portage"
sudo eselect profile set default/linux/amd64/23.0/systemd

sudo-do-sync "${PWD}/var" "/var/lib/portage"

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
