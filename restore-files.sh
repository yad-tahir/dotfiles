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

cd ./polybar/
stow .config -t ~/.config
sudo cp ./system/etc/NetworkManager/dispatcher.d/* /etc/NetworkManager/dispatcher.d/
cd ..

stow lemon-bar
stow sxhkb
stow mpd
stow zathura
stow urxvt
stow monitor
stow git
stow shell

cd ~
rm .emacs.d
ln -s dotfiles/emacs/emacs.d .emacs.d
cd ~/dotfiles/

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

# cd ./pacman/
# sudo rm /lib/systemd/system/pacman-sync.* 2> /dev/null
# sudo cp ./systemd/* /lib/systemd/system/
# sudo stow etc -t /etc
# sudo stow bin -t /bin
# cd ..

cd ./network-manager/
sudo cp ./etc/NetworkManager/dispatcher.d/* /etc/NetworkManager/dispatcher.d/
cd ..

# Install vpn-based apps
sudo rm /bin/firefox-vpn 2> /dev/null
sudo ln -s $DIR/scripts/bin/firefox-vpn /bin/firefox-vpn

# echo "Change the default shell from Bash to Dash"
# sudo rm /bin/sh
# sudo ln -s /bin/dash /bin/sh


cd ./portage/
sudo cp -R ./system/etc/portage/sets /etc/portage/
sudo cp -R ./system/etc/portage/repos.conf /etc/portage/
sudo cp -R ./system/etc/portage/package.use /etc/portage/
sudo cp -R ./system/etc/portage/package.mask /etc/portage/
sudo cp -R ./system/etc/portage/package.accept_keywords /etc/portage/
sudo cp -R ./system/etc/portage/package.license /etc/portage/
sudo cp -R ./system/etc/portage/make.conf /etc/portage/
sudo cp -R ./system/etc/portage/bashrc /etc/portage/
sudo ln -s /home/yad/dotfiles/portage/system/var/lib/portage/world /var/lib/portage/world
sudo ln -sf /home/yad/dotfiles/portage/system/var/lib/portage/world_sets /var/lib/portage/world_sets
sudo rm /var/db/repos/drvegeta
sudo ln -s /home/yad/git/drvegeta-overlay /var/db/repos/drvegeta
cd ..

echo "Done."
