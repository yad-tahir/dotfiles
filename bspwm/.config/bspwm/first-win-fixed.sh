#!/bin/sh
#

# Get the number of windows in the current desktop
num_win=$(bspc query -N -d | wc -l)

# Set the private flag if this is the first window to fix its position
if [ $num_win -eq 0 ] ; then
	echo "private=on"
fi
