#/bin/sh
# Copyright (C) 2026

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

alias cleaor='clear'
alias ls='ls -alih --color'
alias bc='bc --mathlib'
alias cp="cp -v $@"
# alias man='urxvtc -hold -name man -e /usr/bin/man $@'

alias zip-compress='zip -r $1 $@'
alias zip-uncompress='unzip $@'

# Other
if [ -e "$HOME/.config/alias.d/" ]; then
	for f in $HOME/.config/alias.d/* ; do
			 source $f
	done
fi
