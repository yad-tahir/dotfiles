#!/bin/sh

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


# Place environment settings, since it gets executed
# automatically by the DisplayManager during the start-up process
# desktop session as well as by the login shell when one logs in from
# the textual console


# Shortcut commands
alias ls='ls -alih'
alias s="sudo su"
alias e="emacs"
alias ec="emacsclient -cn"
alias web="firefox-developer-edition"
alias pacman-history="expac --timefmt='%Y-%m-%d %T' '%l\t%n' | sort | tail -n 100"
alias pacman-unused='pacman -Qtdq'
alias nv="sudo -u yad -g no-vpn"
alias nV="sudo -u root -g no-vpn"
alias vpn-udp-stop='sudo systemctl stop openvpn-client@personal-udp.service'
alias vpn-udp-start='sudo systemctl restart openvpn-client@personal-udp.service'
alias vpn-tcp-stop='sudo systemctl stop openvpn-client@personal-tcp.service'
alias vpn-tcp-start='sudo systemctl restart openvpn-client@personal-tcp.service'
alias vpn-start='vpn-udp-start'
alias vpn-stop='vpn-udp-stop'
alias vpn-restart='vpn-udp-stop && sleep 1 && vpn-udp-start'
alias nm-stop='nmcli r wifi off'
alias nm-start='nmcli r wifi on'
alias nm-restart='nmcli r wifi off && sleep 1 && nmcli r wifi on'
alias cleaor='clear'

# Other variables
source ~/bin/settings.sh
# Adjust the path
PATH="~/bin:${PATH}"

life

echo ''

# Starts GUI automatically if this is the DISPLAY variable is empty and this is
# the virtual tty 1
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec ~/bin/startx
[[ -z $DISPLAY && $XDG_VTNR -eq 2 ]] && exec ~/bin/startx-nvidia
