#!/bin/sh

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd $DIR

echo "Start stowing!"

mkdir ../bin 2> /dev/null
stow scripts

stow dmenu
stow bspwm
stow lemon-bar
stow sxhkb
stow mpd
stow rofi
stow zathura
stow urxvt
stow monitor
stow x11
stow git
stow shell

sudo stow etc -t /etc

cd ./zfs/
mkdir ../../bin/backup 2> /dev/null
stow bin -t ../../bin
# Enabling symbolic systemd's units can be very buggy. Hence, we need to
# copy the actual files
sudo rm /usr/lib/systemd/system/zfs-backup@.* 2> /dev/null
sudo cp ./systemd/* /usr/lib/systemd/system
cd ..

sudo rm /usr/lib/systemd/system/pacman-sync.*
sudo cp ./pacman/systemd/* /usr/lib/systemd/system/

echo "Done."
