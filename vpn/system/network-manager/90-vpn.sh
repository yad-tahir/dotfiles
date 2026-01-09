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
	echo HELLLO vpn
	# Delete old records
	ip route del default table no-vpn
	ip route del 169.254.0.0/24 table no-vpn
	ip route del 192.168.0.128/25 table no-vpn
	ip route del 192.168.3.0/24 table no-vpn
	ip route del 192.168.2.0/24 table no-vpn

	ip route add 169.254.0.0/24 src 169.254.0.1 dev lxdbr2 table no-vpn
	ip route add 192.168.0.128/25 src 192.168.0.254 dev lxdbr1 table no-vpn
	# ip route add 192.168.3.0/24 dev enp67s0f1 table no-vpn
	# ip route add 192.168.2.0/24 dev enp67s0f0 table no-vpn

	ip route add default via "$currentIP" dev $1 table no-vpn
	ip rule add fwmark 1 table no-vpn prior 1000

	# The status might be not successful after executing the commands above. For
	# example, the ip rule might already exist the table when we try to add it.
	# There is no need to report this error status back to systemd. These
	# exceptions are typical and okay to occur.
	true
}

case "$1" in
	wlp*)
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
