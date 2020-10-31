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

do-ln-sync "${PWD}/local/config" "${HOME}/.config"

TARGET="${HOME}/bin/screen-layout"
[ ! -e "${TARGET}" ] && mkdir --parents "${TARGET}"
do-ln-sync "${PWD}/local/bin/screen-layout/util.sh" "$TARGET/util.sh"
do-ln-sync "${PWD}/local/bin/screen-layout/home-xps.sh" "$TARGET/home.sh"
do-ln-sync "${PWD}/local/bin/screen-layout/laptop-xps.sh" "$TARGET/laptop.sh"
do-ln-sync "${PWD}/local/bin/screen-layout/office-xps.sh" "$TARGET/office.sh"
do-ln-sync "${PWD}/local/bin/screen-layout/lab-xps.sh" "$TARGET/lab.sh"
