#!/bin/sh

if [ "$1" = "wlp4s0" ]; then
case "$2" in
    up)
		# Refresh the local DNS client
		systemctl restart systemd-resolved.service &&
		# Start the VPN
		systemctl start openvpn-client@uk.service
	;;
    down)
		systemctl stop openvpn-client@uk.service;
	;;
    connectivity-change)
		# Refresh the local DNS client
		systemctl restart systemd-resolved.service &&
		# Reconnect the VPN
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
