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
source ../utils.sh

TARGET="${HOME}/bin/dmenu"
[ ! -e "${TARGET}" ] && mkdir --parents "${TARGET}"

do-ln-sync "${PWD}/local/bin/dmenu/dmenu-util" "${TARGET}/dmenu-util"
do-ln-sync "${PWD}/local/bin/dmenu/run.sh" "${TARGET}/run.sh"
do-ln-sync "${PWD}/local/bin/dmenu/pass.sh" "${TARGET}/pass.sh"
do-ln-sync "${PWD}/local/bin/dmenu/monitor-xps.sh" "${TARGET}/monitor.sh"
