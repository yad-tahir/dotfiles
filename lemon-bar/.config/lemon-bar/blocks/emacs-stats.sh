#! /bin/sh
#

. /home/yad/scripts/settings.sh

EMACS_FIFO=${PANEL_FIFO}-emacs
[ -e "$EMACS_FIFO" ] && rm "$EMACS_FIFO"
mkfifo "$EMACS_FIFO" -m600

# Keep the pipeline open
# Source: https://bbs.archlinux.org/viewtopic.php?id=168461
sid=$(sleep 9999d > $EMACS_FIFO &
	  echo $!) &

if [ "$#" -eq 0 ]; then

	while read line; do

		buffer_count="${line#?}"
		
		if [ $buffer_count -gt 30 ]; then
			emacs="%{F$COLOR_INDICATOR3} ${buffer_count}%{F-}"
		else
			emacs=" ${buffer_count}"
		fi

		echo "Se${emacs}"

	done < $EMACS_FIFO

else
	while true; do
		echo "Se${emacs}"
		sleep $1
	done
fi

kill $sid
