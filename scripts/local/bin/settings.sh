#!/bin/bash

# Copyright (C) 2020

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
# $EDITOR and $VISUAL are managed by eselect
export ALTERNATE_EDITOR="nano"
export TERMINAL="urxvtc"
export BROWSER="firefox"

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

# GUI-based sudo
export SUDO_ASKPASS="/home/${USER}/bin/ask-ssh"
