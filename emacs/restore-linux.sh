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

TARGET=${HOME}/.config/emacs
rm ${TARGET} 2> /dev/null
ln -s ${PWD}/local/emacs.d ${TARGET}

do-sync "$PWD/local/alias.d" "$HOME/.config/alias.d"

sudo-do-sync "$PWD/system/usr/share/applications" "/usr/share/applications"
sudo update-desktop-database
