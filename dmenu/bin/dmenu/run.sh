#!/bin/sh

# A small script to prepare a list of commands available in the system,
# then use dmenu to show it.

# Get the settings
. /home/yad/bin/settings.sh


cachedir="$HOME/.cache/"
if [ ! -d "$cachedir" ]; then
	mkdir "$cachedir"
fi

cache=$cachedir/dmenu_run

run=$(
	IFS=:
	# Rebuild the cached list if necessary
	if stest -dqr -n "$cache" $PATH; then
		# Add Emacs and firefox to the beginning of the list
		prefix_commands=("emacs" "firefox-developer-edition")
		echo_string=
		filter_string=
		
		for i in ${prefix_commands[*]}; do
			echo_string="${i}\\n${echo_string}"
			filter_string="${i}\$|${filter_string}"
		done

		{ echo -e "${echo_string}";
		  stest -flx $PATH |
			  awk '!/${filter_string}/' |
			  sort -u; } |
			uniq > "$cache"
	fi
	cat "$cache" | dmenu -i -f -h $PANEL_HEIGHT \
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
		exec $(echo "$TERMINAL -hold -e $run" | sed -e 's/!$//')
		;;
	*?*)
		exec "$run"
		;;
esac
