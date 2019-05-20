#! /bin/sh
#

while true; do
	tmp=$(insync-headless get_status)
	if [ "$tmp" != "SHARE" ]; then
		echo "Sg%{F$COLOR_INDICATOR3} %{F-}"
	else
		echo "Sg"
	fi

	if [ "$#" -eq 0 ]; then
		break
	else
		sleep $1
	fi
done
