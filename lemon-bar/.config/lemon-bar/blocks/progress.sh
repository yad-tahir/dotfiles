#! /bin/sh
#
# A utility script to draw a progress using text underline.

title_text="$1"
position="$2"

# Get text length
len="${#title_text}"

# Calculate the number of characters that needs to be underline
if [ $position -eq $position 2> /dev/null ]; then
	underline_num=$((len*position/100))

	# Create the output
	output="%{+u}"

	i=0
	# Loop to go through underlined chars
	while [ $i -lt $underline_num ]; do
		output="${output}"${title_text:$i:1}
		i=$((i+1))
	done
	output="${output}%{-u}"

	# Another loop to append the rest of the chars
	while [ $i -lt $len ]; do
		output="${output}"${title_text:$i:1}
		i=$((i+1))
	done

	echo $output
fi
