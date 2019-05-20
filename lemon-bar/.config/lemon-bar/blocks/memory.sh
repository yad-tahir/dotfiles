#! /bin/sh
#

while true; do
	tmp=$(echo $(free -h 2> /dev/null | awk '/^Mem/{print substr($3,1,length($3)-1)}'))
	echo "Sm" $tmp

	if [ "$#" -eq 0 ]; then
		break
	else
		sleep $1
	fi
done
