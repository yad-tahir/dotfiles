#!/bin/sh
#

while true; do
	state=$(xset -q | awk '/Caps/ {print $4}')
	if [ $state = "off" ]; then
		echo "Sy%{F${COLOR_ACTIVE}}CAPS%{F-}"
	else
		echo "Sy%{F${COLOR_INDICATOR1}}CAPS%{F-}"
	fi
	if [ "$#" -eq 0 ]; then
		break
	else
		sleep $1
	fi
done
