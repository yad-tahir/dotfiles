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

function setup-vpn-routing () {
	local currentIP=$(ip route | grep default | awk '{print $3}')
	# Setup no-vpn routing table
	ip route del default table no-vpn
	# Add the default root to the no-vpn routing table
	echo "current IP is $currentIP"
	ip route add default via "$currentIP" dev $1 table no-vpn
	ip rule add fwmark 1 table no-vpn prior 1000

	# The status might be not successful after executing the commands above. For
	# example, the ip rule might already exist the table when we try to add it.
	# There is no need to report this error status back to systemd. These
	# exceptions are typical and okay to occur.
	true
}

case "$1" in
	wlp59s0)
		case "$2" in
			up)
				systemctl start openvpn-client@personal-udp.service
				setup-vpn-routing $1
				;;
			down)
				systemctl stop openvpn-client@personal-udp.service
				;;
			connectivity-change)
				systemctl restart openvpn-client@personal-udp.service
				setup-vpn-routing $1
				;;
		esac
		;;
esac
