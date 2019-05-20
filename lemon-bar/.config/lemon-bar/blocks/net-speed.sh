#! /bin/sh
#

. $HOME/bin/settings.sh

while true; do
	ping=$(ping 8.8.8.8 -c 3 2> /dev/null |
			   awk '/avg/{split($4,arr,"/"); printf("%3.0f\n",arr[2])}' |
			   awk '$1=$1' 2> /dev/null)

	if [ $ping ]; then
		if [ $ping -gt 200 ]; then
			printf "%s%-5s%s\n" "Si%{F$COLOR_INDICATOR3} " "${ping}ms" "%{F-}"
		else
			printf "%s%-5s\n" "Si " "${ping}ms"
		fi
	else
			printf "%s%s\n" "Si%{F$COLOR_INDICATOR3}" "%{F-}"
	fi

	if [ "$#" -eq 0 ]; then
		break
	else
		sleep $1
	fi

done
