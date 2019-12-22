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

USER=yad

# General
export EDITOR="emacsclient -c"
export VISUAL="emacsclient -c"
export ALTERNATE_EDITOR="nano"
export TERMINAL="urxvtc"
export BROWSER="firefox-developer-edition"
export JAVA_HOME="/usr/lib/jvm/default"

# mpd
export MPD_HOST="localhost"
export MPD_PORT="6600"

# gpg & ssh
export GNUPGHOME=/home/${USER}/.config/gpg
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)

# Pass
export PASSWORD_STORE_DIR=$HOME/documents/shadows
export PASSWORD_STORE_SIGNING_KEY=$(cat $GNUPGHOME/usage/pass.sign.key)
export PASSWORD_STORE_GENERATED_LENGTH=25

# Terminal
export RXVT_SOCKET=/run/user/1000/urxvtd-pipeline

export PANEL_FONT_FAMILY="DejaVu Sans Mono:size=6"
export PANEL_FONT_FAMILY2="FontAwesome:size=6"
export PANEL_FONT_FAMILY3="DejaVu Sans Mono:size=10"
export PANEL_HEIGHT="35"

# Colors
export COLOR_FOREGROUND='#e6beae'
export COLOR_BACKGROUND='#000000'
export COLOR_INDICATOR1="#a7e23e"
export COLOR_INDICATOR2="#00ff7f"
export COLOR_INDICATOR3="#ffa500"
export COLOR_INDICATOR4="#ff4422"
export COLOR_INDICATOR5="#bd9c8f"
export COLOR_INDICATOR6="#392626"
export COLOR_INDICATOR7="#da70d6"
