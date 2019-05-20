#!/bin/sh

# A small shell script that makes BSPWM slightly more suitable for ultra-wide
# monitors. When there are three windows or less in the currently
# focused desktop, this script automatically equalizes their size.
# This script needs to be executed independently. It cannot be part of
# bspc config 'external_rules_command' as it needs to liten to two events:
# when BSP creates a new node, or when a node has been killed.

. $HOME/bin/settings.sh

# Create a pipeline
ULTRAWIDE_FIFO="/tmp/bspwm-ultrawide-fifo"
[ -e "$ULTRAWIDE_FIFO" ] && rm "$ULTRAWIDE_FIFO"
mkfifo "$ULTRAWIDE_FIFO" -m600
bspc subscribe node > $ULTRAWIDE_FIFO &

while read line ; do
	case $line in
		node_add*|node_remove*)
			size=$(bspc query -N -d | wc -l)

			# Balance when the number of windows is three or less.
			# Since BSPWM uses a binary tree, we need 5 nodes (leafs) to
			# have three windows .
			if [ "${size}" -lt 6 ] ; then
				bspc node any.local -B
			fi
			;;
	esac
done < $ULTRAWIDE_FIFO
