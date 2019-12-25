#/bin/sh
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

alias emerge-sync='sudo emerge --sync && sudo eix-update'
alias emerge-update='sudo emerge --update --deep --with-bdeps\=y @world'
alias emerge-remove='sudo emerge --depclean $@'
alias emerge-install='sudo emerge $@'
alias emerge-list='qlist -I'
alias emerge-has='equery hasuse $@'
alias emerge-dep='equery depends $@'
alias emerge-req='equery depgraph $@'
alias emerge-files='equery files $@'
alias emerge-belong='equery belongs $@'
alias emerge-time='sudo qlop $@'
emerge-info () {
	echo 'Defination:'
	equery which $@ 2> /dev/null
	echo ''
	equery m $@ 2> /dev/null
	echo ''
	eix -e $@ 2> /dev/null
	echo ''
	equery uses $@ 2> /dev/null
	echo ''
	echo 'Size:'
	equery size $@ 2> /dev/null || echo "Not installed"
}
