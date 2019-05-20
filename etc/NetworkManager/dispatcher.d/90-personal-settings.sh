#!/bin/sh

if [ "$1" == "wlp4s0" ]; then
case "$2" in
    up)
		# Start the VPN
		systemctl start openvpn-client@uk.service;
		# Refresh the local DNS client
		systemctl restart systemd-resolved.service;
	;;
    down)
		systemctl stop openvpn-client@uk.service;
	;;
    connectivity-change)
		# Reconnect the VPN
		systemctl restart openvpn-client@uk.service;
		# Refresh the local DNS client
		systemctl restart systemd-resolved.service;
	;;
esac
fi

# Notify Lemon Bar
/home/yad/.config/lemon-bar/blocks/public-ip.sh | sudo -u yad tee /tmp/lemon-panel-fifo > /dev/null
/home/yad/.config/lemon-bar/blocks/net-speed.sh | sudo -u yad tee /tmp/lemon-panel-fifo > /dev/null
