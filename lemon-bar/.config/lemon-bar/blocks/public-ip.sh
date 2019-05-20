#!/bin/sh
#

. /home/yad/scripts/settings.sh

while true; do
	code=$(curl ipinfo.io 2> /dev/null | awk '/country/ {print substr($2,2,2)}' 2> /dev/null)
	if [ "$code" == "GB" ]; then
		code=" ${code}"
	elif [ "$code" == "" ]; then
		code="%{F${COLOR_INDICATOR3}}%{F-}"
	else
		code="%{F${COLOR_INDICATOR3}} ${code}%{F-}"
	fi

	echo "Sn${code}"

	if [ "$#" -eq 0 ]; then
		break
	else
		sleep $1
	fi
done
