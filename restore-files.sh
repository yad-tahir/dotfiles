#!/bin/sh

# Copyright (C) 2019

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

echo "Start stowing!"

mkdir ../bin 2> /dev/null
stow scripts

stow dmenu
stow bspwm
stow lemon-bar
stow sxhkb
stow mpd
stow zathura
stow urxvt
stow monitor
stow x11
stow git
stow shell

cd ./zfs/
sudo stow bin -t /bin
# Enabling symbolic systemd's units can be very buggy. Hence, we need to
# copy the actual files
sudo rm /usr/lib/systemd/system/zfs-backup@.* 2> /dev/null
sudo cp ./systemd/* /usr/lib/systemd/system
cd ..

cd ./power-management/
sudo cp ./system/etc/tmpfiles.d/* /etc/tmpfiles.d/
sudo cp ./system/etc/udev/* /etc/udev/rules.d/
sudo cp ./system/usr/bin/* /usr/bin/
cd ..

cd ./pacman/
sudo rm /usr/lib/systemd/system/pacman-sync.* 2> /dev/null
sudo cp ./systemd/* /usr/lib/systemd/system/
sudo stow etc -t /etc
sudo stow bin -t /bin
cd ..

cd ./network-manager/
sudo stow etc -t /etc
cd ..
# Install vpn-based apps
sudo rm /bin/firefox-vpn 2> /dev/null
sudo ln -s $DIR/scripts/bin/firefox-vpn /bin/firefox-vpn

echo "Change the default shell from Bash to Dash"
sudo rm /bin/sh
sudo ln -s /bin/dash /bin/sh

echo "Done."
