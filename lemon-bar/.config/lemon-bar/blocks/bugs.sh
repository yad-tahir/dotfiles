#! /bin/sh
#


. /home/yad/scripts/settings.sh

function print_num_packages {
	c=$(journalctl -b -p err --no-pager --no-tail --no-full | wc -l)
	if [ $c -gt 30 ]; then
		c="%{B$COLOR_INDICATOR4}%{F$COLOR_BACKGROUND}"$c"%{F-}%{B-}"
	else
		c=" "$c
	fi
	echo "Sb" $c
}

if [ "$#" -eq 0 ]; then
	BUGS_FIFO=${PANEL_FIFO}-bugs
	[ -e "$BUGS_FIFO" ] && rm "$BUGS_FIFO"
	mkfifo "$BUGS_FIFO" -m600
	journalctl -f -b -p err --no-pager --no-tail --no-full > $BUGS_FIFO &

	while read line ; do
		print_num_packages
	done < $BUGS_FIFO
else
	while true; do
		print_num_packages
		sleep $1
	done
fi
