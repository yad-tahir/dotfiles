#!/bin/bash

# Copyright (C) 2019

# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301, USA.

# General
export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -c"          # $EDITOR opens in terminal
export VISUAL="emacsclient -c"         # $VISUAL opens in GUI mode
export TERMINAL="urxvtc"
export BROWSER="firefox-developer-edition"
export JAVA_HOME="/usr/lib/jvm/default"
export USERNAME="yad" # @TODO: Fix this
					  # To make it also available outside of your user
					  # session, add it to /etc/environment

# mpd
export MPD_HOST="localhost"
export MPD_PORT="6600"

# gpg & ssh
export GNUPGHOME=/home/${USERNAME}/.config/gpg
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)

# Pass
export PASSWORD_STORE_DIR=$HOME/documents/shadows
export PASSWORD_STORE_SIGNING_KEY=$(cat $GNUPGHOME/usage/pass.sign.key)
export PASSWORD_STORE_GENERATED_LENGTH=25

# Terminal
export RXVT_SOCKET=/run/user/1000/urxvtd-arch-laptop

#Lemon-bar
export PANEL_FONT_FAMILY="DejaVu Sans Mono-6"
export PANEL_FONT_FAMILY2="FontAwesome-7"
export PANEL_HEIGHT="35"
export PANEL_WM_NAME=lemon_bspwm_panel

export PANEL_FIFO="/tmp/lemon-panel-fifo"

# Colors
export COLOR_FOREGROUND='#e6beae'
export COLOR_BACKGROUND='#0f0b0c'
export COLOR_ACTIVE="#392626"
export COLOR_ACTIVE2="#bd9c8f"
export COLOR_INDICATOR1="#a7e23e"
export COLOR_INDICATOR2="#6fd8ff"
export COLOR_INDICATOR3="#ffa500"
export COLOR_INDICATOR4="#ff4422"

export COLOR_TITLE_FG=$COLOR_FOREGROUND
export COLOR_TITLE_BG=$COLOR_BACKGROUND
export COLOR_MONITOR_FG=$COLOR_FOREGROUND
export COLOR_MONITOR_BG=$COLOR_BACKGROUND
export COLOR_MONITOR_ACTIVE_FG=$COLOR_FOREGROUND
export COLOR_MONITOR_ACTIVE_BG=$COLOR_BACKGROUND
export COLOR_MONITOR_FOCUSED_FG=$COLOR_INDICATOR3
export COLOR_MONITOR_FOCUSED_BG=$COLOR_BACKGROUND
export COLOR_DESKTOP_FG=$COLOR_FOREGROUND
export COLOR_DESKTOP_BG=$COLOR_BACKGROUND
export COLOR_DESKTOP_ACTIVE_FG=$COLOR_FOREGROUND
export COLOR_DESKTOP_ACTIVE_BG=$COLOR_ACTIVE
export COLOR_DESKTOP_FOCUSED_FG=$COLOR_BACKGROUND
export COLOR_DESKTOP_FOCUSED_BG=$COLOR_INDICATOR1
export COLOR_DESKTOP_URGENT_FG=$COLOR_BACKGROUND
export COLOR_DESKTOP_URGENT_BG=$COLOR_INDICATOR3


