#!/bin/sh


while true; do
	ACPI=$(acpi)
	CODE=$(echo $ACPI | awk '{print tolower(substr($3,1,length($3)-1))}')

	if [ $CODE = "discharging" ]
	then
		BAT=$(echo $ACPI | awk '{print substr($4,1,length($4)-2)}')
		if [ $BAT -ge 90 ];then
			B=$COLOR_INDICATOR2
			F=$COLOR_BACKGROUND
		elif [ $BAT -ge 60 ]; then
			B=$COLOR_INDICATOR1
			F=$COLOR_BACKGROUND
		else
			B=$COLOR_INDICATOR4
			F=$COLOR_BACKGROUND
		fi
		echo "Sx%{B$B}%{F$F} "$BAT"%{F-}%{B-}"
	else
		echo "Sx"
	fi

	if [ "$#" -eq 0 ]; then
		break
	else
		sleep $1
	fi
done
