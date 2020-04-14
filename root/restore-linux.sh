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

# For security reasons, avoid creating soft links as much as possible
sudo mkdir /root/bin 2> /dev/null
do-sync-sudo "$PWD/../shell/other/command-prompt.sh" "/root/bin/command-prompt.sh"
do-sync-sudo "$PWD/local/.profile" "/root/.profile"
do-sync-sudo "$PWD/local/.bashrc" "/root/.bashrc"
sudo rm /root/.emacs.d 2> /dev/null
sudo ln -s "/home/yad/dotfiles/emacs/local/emacs.d" "/root/.emacs.d"
