#!/bin/sh
#

while true; do
	state=$(pulsemixer --get-volume | awk '{print $1}')
	if [ $state -gt 100 ]; then
		printf "%s%-7.7s%s\n" "Sv%{F${COLOR_BACKGROUND}}%{B${COLOR_INDICATOR4}}" " ${state}" "%{B-}%{F-}"
	elif  [ $state -gt 50 ]; then
		printf "%s%-7.7s%s\n" "Sv%{F${COLOR_FOREGROUND}}%{B${COLOR_BACKGROUND}}" " ${state}" "%{B-}%{F-}"
	elif  [ $state -gt 0 ]; then
		printf "%s%-7.7s%s\n" "Sv%{F${COLOR_FOREGROUND}}%{B${COLOR_BACKGROUND}}" " ${state}" "%{B-}%{F-}"
	else
		printf "%s%-7.7s%s\n" "Sv%{F${COLOR_INDICATOR3}}%{B${COLOR_BACKGROUND}}" " ${state}" "%{B-}%{F-}"
	fi
	if [ "$#" -eq 0 ]; then
		break
	else
		sleep $1
	fi
done
