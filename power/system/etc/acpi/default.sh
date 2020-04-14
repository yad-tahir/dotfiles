#!/bin/bash

set $*

group=${1%%/*}
action=${1#*/}
device=$2
id=$3
value=$4

function log_unhandled {
	local no_debuging=1 # A dumb statement to workaround Bash's body function requirement
	# logger "ACPI event unhandled: $*"
}

case "$group" in
	button)
		case "$action" in
			power)
				/etc/acpi/actions/powerbtn.sh
				;;
			lid)
				if [ "$id" == "open" ]; then
					XAUTHORITY=/home/yad/.Xauthority DISPLAY=:0 \
							  /usr/bin/xrandr \
							  --output eDP-1 \
							  --auto \
							  --brightness 0.5
				fi
				;;
			*)	log_unhandled $* ;;
		esac
		;;

	video)
		LEVEL=/tmp/monitor-brightness
		[ ! -f $LEVEL ] && echo '0.5' > $LEVEL
		case "$action" in
			brightnessup)
				C=$(echo $(cat $LEVEL) + 0.05 | bc)
				if [ ! "$C" == "1.0" ]; then
				   echo $C > $LEVEL
				   XAUTHORITY=/home/yad/.Xauthority DISPLAY=:0 \
							 /usr/bin/xrandr \
							 --output eDP-1 \
							 --auto \
							 --brightness $C
			   fi
			;;
			brightnessdown)
				C=$(echo $(cat $LEVEL) - 0.05 | bc)
				if [ ! "$C" == "-.05" ]; then
				   echo $C > $LEVEL
				   XAUTHORITY=/home/yad/.Xauthority DISPLAY=:0 \
							 /usr/bin/xrandr \
							 --output eDP-1 \
							 --auto \
							 --brightness $C
			   fi
			esac
		;;

	ac_adapter)
		case "$value" in
			*)	log_unhandled $* ;;
		esac
		;;
	"9DBB5994-A997-")
		if [ $(/usr/bin/nmcli radio wifi) == 'enabled' ]; then
			/usr/bin/nmcli radio wifi off
		else
			/usr/bin/nmcli radio wifi on
		fi
		;;

	*)	log_unhandled $* ;;
esac
