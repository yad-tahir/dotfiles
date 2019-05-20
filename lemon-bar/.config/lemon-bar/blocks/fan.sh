#! /bin/sh
#

while true; do
	tmp=$(echo $(sensors 2> /dev/null | awk '/^Left/{print $4}/^Right/{print $4}'))
	echo "Sf" $tmp

	if [ "$#" -eq 0 ]; then
		break
	else
		sleep $1
	fi
done
