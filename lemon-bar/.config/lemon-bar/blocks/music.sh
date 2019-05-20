#! /bin/sh
#

current=0
len=20

while true; do
	state=$(mpc current -f '%title% %artist% %album%' 2> /dev/null)
	pause=$(mpc status | awk '/paused/{print $0}' 2> /dev/null)

	if [ "$state" == "" -o "$pause" != "" ]; then
		echo "Sw "
		break
	else
		position=$(mpc status | awk '/playing/{print $3}' 2> /dev/null)
		title=" $position $state"

		percent=$(mpc status | awk '/playing/{print substr($4,2,length($4)-3)}' 2> /dev/null)
		text=$($HOME/.config/lemon-bar/blocks/progress.sh " $title " $percent)

		# print the formatted text
		echo "Sw${text}"
	fi

	if [ "$#" -eq 0 ]; then
		break
	else
		sleep $1
	fi

done
