#!/bin/bash

# Modify from https://git.zx2c4.com/password-store/tree/contrib/dmenu/passmenu

. $HOME/bin/settings.sh

prefix=${PASSWORD_STORE_DIR}
# Get the files
password_files=( "$prefix"/**/*.gpg  "$prefix"/**/**/*.gpg )
# Remove the path and .gpg extension
password_files=( "${password_files[@]#"$prefix"/}" )
password_files=( "${password_files[@]%.gpg}" )

value=$(printf '%s\n' "${password_files[@]}" |
			dmenu -i -f -h $PANEL_HEIGHT \
				  -nb "$COLOR_BACKGROUND" \
				  -nf "$COLOR_FOREGROUND" \
				  -sb "$COLOR_INDICATOR4" \
				  -sf "$COLOR_BACKGROUND" \
				  -fn "$PANEL_FONT_FAMILY" \
				  -l 0 -p "Password" "$@")

[[ -n $value ]] || exit

sudo -u $USER -g no_net /bin/sh -c ". $HOME/bin/settings.sh && pass -c \"$value\""
