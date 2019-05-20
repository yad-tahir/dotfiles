#!/bin/sh

. $HOME/bin/settings.sh

state=$(setxkbmap -query |
				awk '/^layout/{split($2,arr,","); print toupper(arr[1])}')

if [ $state = "DVORAK" ]; then
	setxkbmap us
	setxkbmap -option
else
	setxkbmap dvorak
	# setxkbmap -option altwin:swap_alt_win
fi

~/.config/lemon-bar/blocks/keyboard.sh > $PANEL_FIFO
