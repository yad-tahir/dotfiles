#!/bin/bash

ACPI=$(acpi)
CODE=$(echo $ACPI | awk '{print tolower(substr($3,1,length($3)-1))}')

if [ $CODE == "discharging" ]
then
	BAT=$(echo $ACPI | awk '{print substr($4,1,length($4)-1)}')

	echo  $CODE $BAT
	echo  $CODE

	if [ $BAT -ge 70 ];then
		echo "#a7e23e"
	elif [ $BAT -ge 40 ]; then
		echo "#ff6347"
	else
		exit 33
	fi
else
	echo 
	echo 
fi
