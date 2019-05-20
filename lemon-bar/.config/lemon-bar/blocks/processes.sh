#! /bin/sh
#

while true; do
	tmp=$(ps axl | awk '$7 != 0 && $10 !~ "Z"'| wc -l)
	tmp2=$(ps axl -L | awk '$7 != 0 && $10 !~ "Z"'| wc -l)
	echo "Sp" $tmp " " $tmp2

	if [ "$#" -eq 0 ]; then
		break
	else
		sleep $1
	fi
done
