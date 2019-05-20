#!/bin/sh

echo "Start stowing!"

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
stow scripts -t $HOME/scripts
sudo stow root -t /
cd ..


echo "Done."
