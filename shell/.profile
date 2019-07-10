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
alias ls='ls -ali'
alias s="sudo su"
alias e="emacs"
alias ec="emacsclient -cn"
alias firefox="firefox-developer-edition"
alias pacman-history="expac --timefmt='%Y-%m-%d %T' '%l\t%n' | sort | tail -n 100"
alias pacman-unused='pacman -Qtdq'
alias kweb='sudo -u yad -g no_net KeeWeb'
alias cleaor='clear'

# Other variables
source ~/bin/settings.sh

# Adjust the path
PATH="~/bin:${PATH}"

life
echo ''
