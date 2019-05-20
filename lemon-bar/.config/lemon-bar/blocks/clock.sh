#! /bin/sh
#

while true; do
	echo Sc$(date '+%A, %B %d %H:%M:%S ')

	if [ "$#" -eq 0 ]; then
		break
	else
		sleep $1
	fi
done
