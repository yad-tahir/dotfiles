#!/bin/sh

# A small menu to launch an Emacs command

# Get the settings
. /home/yad/scripts/settings.sh

emacs_commands=("do-capture"
				"do-agenda"
				"do-music-playlist"
				"do-scratch-buffer"
				"do-calculator"
				"do-notebook"
				"do-file-manager")

echo_string=
for i in ${emacs_commands[*]}; do
	echo_string="${i}\\n${echo_string}"
done

run=$(echo -e "${echo_string}" |
		  dmenu -i -f -h $PANEL_HEIGHT \
				-nb "$COLOR_BACKGROUND" \
				-nf "$COLOR_FOREGROUND" \
				-sb "$COLOR_INDICATOR1" \
				-sf "$COLOR_BACKGROUND" \
				-fn "$PANEL_FONT_FAMILY" \
				-l 0 -p "Emacs" "$@")


if [ ! -z "${run}" -a "${run}" != "" ]; then
	exec emacsclient -qne "(${run})" > /dev/null &
fi
