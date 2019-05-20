#!/bin/sh

# A small menu to choose the monitor setup

# Get the settings
. $HOME/bin/settings.sh


layouts="laptop lab office home"

for i in $layouts; do
	echo_string="${i}\\n${echo_string}"
done

run=$(echo "${echo_string}" |
		  dmenu -i -f -h $PANEL_HEIGHT \
				-nb "$COLOR_BACKGROUND" \
				-nf "$COLOR_FOREGROUND" \
				-sb "$COLOR_INDICATOR2" \
				-sf "$COLOR_BACKGROUND" \
				-fn "$PANEL_FONT_FAMILY" \
				-l 0 -p "Monitor" "$@")


if [ ! -z "${run}" -a "${run}" != "" ]; then
	exec ~/bin/screen-layout/"${run}.sh" > /dev/null
fi
