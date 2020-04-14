#!/bin/sh

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

DIR="$HOME/dotfiles"
cd $DIR

mkdir ${HOME}/bin 2> /dev/null

./emacs/restore-linux.sh &
./dmenu/restore-linux.sh &
./dwm/restore-linux.sh &
./firewall/restore-linux.sh &
./gdrive/restore-linux.sh &
./git/restore-linux.sh &
./igpu/restore-linux.sh &
./logger/restore-linux.sh &
./kernel/restore-linux.sh &
./monitor/restore-linux.sh &
./mpd/restore-linux.sh &
./network/restore-linux.sh &
./nvidia/restore-linux.sh &
# ./pacman/restore-linux.sh &
./portage/restore-linux.sh &
./power/restore-linux.sh &
./root/restore-linux.sh &
./scripts/restore-linux.sh &
./shell/restore-linux.sh &
./visualizer/restore-linux.sh &
./urxvt/restore-linux.sh &
./vm/restore-linux.sh &
./vpn/restore-linux.sh &
./x11/restore-linux.sh &
./zathura/restore-linux.sh &
./zfs/restore-linux.sh &

wait
echo "Done."
