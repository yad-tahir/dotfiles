#! /bin/sh
#

while true; do
	temp=$(sensors | awk '/^Package/{print substr($4,2,length($4)-5)}')
	if [ $temp -gt 100 ]; then
		temp="%{B$COLOR_INDICATOR4}%{F$COLOR_BACKGROUND} "$temp"°C %{F-}%{B-}"
	elif [ $temp -gt 85 ]; then
		temp="%{B$COLOR_BACKGROUND}%{F$COLOR_INDICATOR3} "$temp"°C %{F-}%{B-}"
	else
		temp="%{B$COLOR_BACKGROUND}%{F$COLOR_FOREGROUND} "$temp"°C %{F-}%{B-}"
	fi
	echo "St" $temp

	if [ "$#" -eq 0 ]; then
		break
	else
		sleep $1
	fi
done
