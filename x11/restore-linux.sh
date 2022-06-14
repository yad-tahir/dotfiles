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

dst="${HOME}/.config"
do-sync "${PWD}/local/config" "$dst"

src=${PWD}/local/Xresources
do-sync "$src" "${HOME}" ".Xresources"

src=${PWD}/local/xinitrc
do-sync "$src" "${HOME}" ".xinitrc"

dst=/etc/systemd/system/getty@tty1.service.d
src=${PWD}/system${dst}
sudo-do-sync "$src" "$dst"

dst=/etc/systemd/system/getty@tty2.service.d
src=${PWD}/system${dst}
sudo-do-sync "$src" "$dst"

dst=/etc/X11/xorg.conf.d
src=${PWD}/system${dst}
sudo-do-sync "$src" "$dst"

dst=/etc/X11/xinit
src=${PWD}/system${dst}
sudo-do-sync "$src" "$dst"
