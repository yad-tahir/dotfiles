#!/bin/sh

# A small script to prepare a list of commands available in the system,
# then use dmenu to show it.

# Get the settings
. $HOME/bin/settings.sh

run=$(
		IFS='|'
		# Add Emacs and firefox to the beginning of the list
		prefix_commands="emacs|firefox-developer-edition"
		echo_string=
		filter_string=
		
		for i in $prefix_commands; do
			echo_string="${i}\\n${echo_string}"
			filter_string="${i}\$|${filter_string}"
		done
		# Delete the last character
		filter_string=$(awk -v VAL="$filter_string" \
						'BEGIN {print substr(VAL,0,length(VAL)-1)}')
		IFS=:
		{ echo "${echo_string}";
		  stest -flx $PATH |
			  awk "!/${filter_string}/" |
			  sort -u | uniq ; } |
			dmenu -i -f -h $PANEL_HEIGHT \
						 -nb "$COLOR_BACKGROUND" \
						 -nf "$COLOR_FOREGROUND" \
						 -sb "$COLOR_INDICATOR3" \
						 -sf "$COLOR_BACKGROUND" \
						 -fn "$PANEL_FONT_FAMILY" \
						 -l 0 -p "App" "$@"
   )



#Run the selected command
case "$run" in
	# If it ends with '!', run it in a terminal instead
	*!)
		$(echo "$TERMINAL -hold -e $run" | sed -e 's/!$//') &
		;;
	*?*)
		"$run" &
		;;
esac
