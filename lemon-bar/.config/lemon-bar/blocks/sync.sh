#! /bin/sh
#

while true; do
	tmp=$(insync-headless get_syn_progress | head -n1)

	if [ "$tmp" != "No syning activities" ]; then
		echo "Sg"
	elif [ "$tmp" == "Uploading" ]; then
		echo "Sg%{F$COLOR_INDICATOR3} %{F-}"
	else
		echo "Sg%{F$COLOR_INDICATOR2} %{F-}"
	fi

	if [ "$#" -eq 0 ]; then
		break
	else
		sleep $1
	fi
done
