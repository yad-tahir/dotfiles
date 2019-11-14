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

case "$1" in
	wlp59s0)
		case "$2" in
			up)
				# OpenVPN may require DNS. A quick reboot is needed to make sure the DNS daemon is
				# well initialized.
				# systemctl restart systemd-resolved.service
				systemctl start openvpn-client@personal-udp.service

				# Setup no-vpn routing table
				ip route del default table no-vpn 2> /dev/null
				# Add the default root to the no-vpn routing table
				ip route add default via $(ip route | grep default | awk '{print $3}') dev $1 table no-vpn 2> /dev/null
				ip rule add fwmark 1 table no-vpn prior 1000
				;;
			down)
				systemctl stop openvpn-client@personal-udp.service
				;;
			connectivity-change)
				systemctl restart openvpn-client@personal-udp.service
				# Add the default root to the no-vpn routing table
				ip route del default table no-vpn 2> /dev/null
				ip route add default via $(ip route | grep default | awk '{print $3}') dev $1 table no-vpn 2> /dev/null
				;;
		esac
		;;
esac

# Notify Lemon Bar
USERNAME=yad #@TODO: Remove it. For some reasons, the Network Manager does not load
#			 # the variables in /etc/environment!

. /home/$USERNAME/bin/settings.sh
sudo -u $USERNAME /bin/sh -c "/home/$USERNAME/.config/lemon-bar/blocks/net-speed.sh > $PANEL_FIFO" &

sudo -u $USERNAME /bin/sh -c "/home/$USERNAME/.config/lemon-bar/blocks/public-ip.sh > $PANEL_FIFO" &
