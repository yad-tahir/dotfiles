#!/bin/sh

if [ "$1" == "wlp4s0" ]; then
case "$2" in
    up)
		# Refresh the local DNS client
		systemctl restart systemd-resolved.service;
		# Start the VPN
		systemctl start openvpn-client@uk.service;
	;;
    down)
		systemctl stop openvpn-client@uk.service;
	;;
    connectivity-change)
		# Refresh the local DNS client
		systemctl restart systemd-resolved.service;
		# Reconnect the VPN
		systemctl restart openvpn-client@uk.service;
	;;
esac
fi

# Notify Lemon Bar
/home/yad/.config/lemon-bar/blocks/public-ip.sh |
	sudo -u yad tee /tmp/lemon-panel-fifo &> /dev/null &
/home/yad/.config/lemon-bar/blocks/net-speed.sh |
	sudo -u yad tee /tmp/lemon-panel-fifo &> /dev/null &
