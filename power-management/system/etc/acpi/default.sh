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

	ac_adapter)
		case "$value" in
			*)	log_unhandled $* ;;
		esac
		;;

	*)	log_unhandled $* ;;
esac
