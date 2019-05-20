#!/bin/sh
#

while true; do
	state=$(setxkbmap -query | awk '/^layout/{split($2,arr,","); print toupper(arr[1])}')
	if [ $state = "DEVORK" ]; then
		echo "Sk%{F${COLOR_INDICATOR1}}$state%{F-}"
	else
		echo "Sk$state"
	fi
	if [ "$#" -eq 0 ]; then
		break
	else
		sleep $1
	fi
done
