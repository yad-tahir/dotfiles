#!/bin/sh

echo "Start stowing!"

mkdir $HOME/scripts 2> /dev/null
stow scripts

stow bspwm
stow lemon-bar
stow sxhkb
stow mpd
stow rofi
stow zathura
stow systemd
stow icc-profiles
stow i3
stow x11
stow git
stow shell

sudo stow etc -t /etc

cd ./zfs/
mkdir $HOME/scripts/backup 2> /dev/null
stow scripts -t $HOME/scripts
sudo stow root -t /
cd ..


echo "Done."
