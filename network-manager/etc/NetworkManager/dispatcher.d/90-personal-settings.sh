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

if [ "$1" = "wlp4s0" ]; then
case "$2" in
    up)
		# OpenVPN requires DNS. A restart is needed to make sure the DNS daemon is
		# well initialized.
		systemctl restart systemd-resolved.service &&
		systemctl start openvpn-client@uk.service
	;;
    down)
		systemctl restart systemd-resolved.service &&
		systemctl stop openvpn-client@uk.service;
	;;
    connectivity-change)
		systemctl restart systemd-resolved.service &&
		systemctl restart openvpn-client@uk.service
	;;
esac
fi

# Notify Lemon Bar
USERNAME=yad #@TODO: Remove it. For some reasons, the Network Manager does not load
# 			 # the variables in /etc/environment!

. /home/$USERNAME/bin/settings.sh
sudo -u $USERNAME /bin/sh -c "/home/$USERNAME/.config/lemon-bar/blocks/public-ip.sh > $PANEL_FIFO" &

sudo -u $USERNAME /bin/sh -c "/home/$USERNAME/.config/lemon-bar/blocks/net-speed.sh > $PANEL_FIFO" &
