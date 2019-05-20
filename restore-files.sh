#!/bin/sh

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

cd ./pacman/
sudo rm /usr/lib/systemd/system/pacman-sync.* 2> /dev/null
sudo cp ./systemd/* /usr/lib/systemd/system/
sudo stow etc -t /etc
sudo stow bin -t /bin
cd ..

cd ./network-manager/
sudo stow etc -t /etc
cd ..

echo "Done."
