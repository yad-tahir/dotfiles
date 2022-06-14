#!/bin/sh

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


# Place environment settings, since it gets executed
# automatically by the DisplayManager during the start-up process
# desktop session as well as by the login shell when one logs in from
# the textual console

. /etc/profile

stty -ixon # Disable ctrl-s and ctrl-q

source $HOME/dotfiles/shell/other/aliases.sh

# Other variables
source $HOME/bin/settings.sh

# Starts GUI automatically if this is the DISPLAY variable is empty, we are on
# the virtual tty 1, and this is the first login after a cold boot
if [[ -z $DISPLAY && $XDG_VTNR -eq 1 && ! -f /tmp/init-startx ]]; then
	touch /tmp/init-startx
	exec startx
fi
