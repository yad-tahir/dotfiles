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

dst="/etc/portage"
src="${PWD}/system${dst}/common-lxd.conf"
sudo-do-sync "$src" "$dst" "common.conf"

dst="/etc/portage/package.use/core-lxd"
src="${PWD}/system${dst}"
sudo-do-sync "$src" "/etc/portage/package.use"

dst="/etc/portage/package.mask/lxd.mask"
src="${PWD}/system${dst}"
sudo-do-sync "$src" "/etc/portage/package.mask"

dst="/etc/portage/package.accept_keywords/lxd"
src="${PWD}/system${dst}"
sudo-do-sync "$src" "/etc/portage/package.accept_keywords"

dst="/etc/portage"
src="${PWD}/system/etc/portage/make-vault-lxd.conf"
sudo-do-sync "$src" "$dst" "make.conf"

touch "/etc/portage/package.use/zz-required"
