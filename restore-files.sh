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
rm ~/.config/lemon-bar -R
mkdir ~/.config/lemon-bar
mkdir ~/.config/lemon-bar/blocks
stow lemon-bar
stow sxhkb
stow mpd
stow zathura
stow urxvt
stow monitor
stow git
stow shell

cd ./emacs/
ln -fs ${PWD}/emacs.d ~/.emacs.d
cd ..

cd ./zfs/
sudo stow bin -t /bin
# Enabling symbolic systemd's units can be very buggy. Hence, we need to
# copy the actual files
sudo rm /lib/systemd/system/zfs-backup*@* 2> /dev/null
sudo cp ./systemd/zfs-backup*@* /lib/systemd/system
cd ..

cd ./power-management/
sudo cp ./system/etc/tmpfiles.d/* /etc/tmpfiles.d/
sudo cp ./system/etc/udev/* /etc/udev/rules.d/
sudo cp ./system/usr/bin/* /usr/bin/
cd ..

cd ./nvidia/
stow .config -t ~/.config
stow bin -t ~/bin
sudo cp ./system/etc/tmpfiles.d/* /etc/tmpfiles.d/
sudo cp ./system/lib/systemd/system/* /lib/systemd/system/
sudo cp ./system/etc/modprobe.d/* /etc/modprobe.d/
sudo cp ./system/usr/sbin/* /usr/sbin/
cd ..

cd ./igpu/
stow bin -t ~/bin
sudo cp ./system/etc/X11/xorg.conf.d/* /etc/X11/xorg.conf.d/
sudo cp ./system/etc/modprobe.d/* /etc/modprobe.d/
cd ..

cd ./x11/
stow .config -t ~/.config
sudo cp ./system/etc/X11/xinit/* /etc/X11/xinit/
sudo cp -R ./system/etc/systemd/system/getty@tty*.service.d /etc/systemd/system/
cd ..

cd ./firewall/
sudo cp ./system/etc/*.conf /etc/
sudo cp ./system/etc/iproute2/* /etc/iproute2/
sudo cp ./system/etc/modprobe.d/* /etc/modprobe.d/
sudo cp ./system/systemd/nftables.service /lib/systemd/system
cd ..

cd ./pacman/
sudo rm /lib/systemd/system/pacman-sync.* 2> /dev/null
sudo cp ./systemd/* /lib/systemd/system/
sudo stow etc -t /etc
sudo stow bin -t /bin
cd ..

cd ./network-manager/
sudo stow etc -t /etc
cd ..
# Install vpn-based apps
sudo rm /bin/firefox-vpn 2> /dev/null
sudo ln -s $DIR/scripts/bin/firefox-vpn /bin/firefox-vpn

# echo "Change the default shell from Bash to Dash"
# sudo rm /bin/sh
# sudo ln -s /bin/dash /bin/sh

echo "Done."
