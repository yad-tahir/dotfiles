#!/bin/sh

echo "Start stowing!"

echo "Restore the home directory"
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

echo "Restore /etc"
sudo stow etc -t /etc


echo "Done."
