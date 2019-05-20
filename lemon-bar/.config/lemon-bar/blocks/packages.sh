#!/bin/sh

while true; do
	tmp=$(pacman -Q | wc -l)
	tmp2=$(pacman -Qu | wc -l)

	[ -e "$PANEL_FIFO" ] && echo "Sj" $tmp " " $tmp2

	if [ "$#" -eq 0 ]; then
		break
	else
		sleep $1
	fi
done
