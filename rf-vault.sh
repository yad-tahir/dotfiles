#!/bin/sh

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

mkdir ${HOME}/bin 2> /dev/null

echo sudo is required. Please enter your password.
sudo /bin/true
if [ $? -ne 0 ]; then
	exit $?
fi

emacs/restore-linux.sh
portage/restore-vault.sh
git/restore-linux.sh
dwm/restore-vault.sh
dmenu/restore-vault.sh
keyboard/restore-linux.sh
logger/restore-linux.sh
firewall/restore-vault.sh
vpn/restore-linux.sh
network/restore-linux.sh
zathura/restore-linux.sh
urxvt/restore-linux.sh
mpd/restore-linux.sh
shell/restore-linux.sh
scripts/restore-vault.sh
root/restore-linux.sh
pam/restore-linux.sh

# ./radeon/restore-linux.sh
# ./sound/restore-linux.sh
# ./x11/restore-linux.sh

echo "Done."
