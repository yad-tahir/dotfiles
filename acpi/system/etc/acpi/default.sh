#!/bin/sh
# /etc/acpi/default.sh
# Default acpi script that takes an entry for all actions

set $*

group=${1%%/*}
action=${1#*/}
device=$2
id=$3
value=$4

log_unhandled() {
	logger "ACPI event unhandled: $*"
}

case "$group" in
	button)
		case "$action" in
			power)
				/etc/acpi/actions/powerbtn.sh
				;;
			lid)
				if [ "$id" == "open" ]; then
						logger "$action is $id. Turn on the OLED screen"
						XAUTHORITY=/home/yad/.Xauthority DISPLAY=:0 /usr/bin/xrandr --output eDP-1 --auto --brightness 0.5
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
				   XAUTHORITY=/home/yad/.Xauthority DISPLAY=:0 /usr/bin/xrandr --output eDP-1 --auto --brightness $C
			   fi
			;;
			brightnessdown)
				C=$(echo $(cat $LEVEL) - 0.05 | bc)
				if [ ! "$C" == "-.05" ]; then
				   echo $C > $LEVEL
				   XAUTHORITY=/home/yad/.Xauthority DISPLAY=:0 /usr/bin/xrandr --output eDP-1 --auto --brightness $C
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
