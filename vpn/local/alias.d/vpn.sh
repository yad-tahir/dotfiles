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

alias nv="sudo -u yad -g no-vpn"
alias nV="sudo -u root -g no-vpn"
alias vpn-udp-stop='sudo systemctl stop openvpn-client@personal-udp.service'
alias vpn-udp-start='sudo systemctl start openvpn-client@personal-udp.service'
alias vpn-udp-restart='sudo systemctl restart openvpn-client@personal-udp.service'
alias vpn-udp-mask='sudo systemctl mask openvpn-client@personal-udp.service'
alias vpn-udp-unmask='sudo systemctl unmask openvpn-client@personal-udp.service'
alias vpn-tcp-stop='sudo systemctl stop openvpn-client@personal-tcp.service'
alias vpn-tcp-start='sudo systemctl start openvpn-client@personal-tcp.service'
alias vpn-tcp-restart='sudo systemctl restart openvpn-client@personal-tcp.service'
alias vpn-tcp-mask='sudo systemctl mask openvpn-client@personal-tcp.service'
alias vpn-tcp-unmask='sudo systemctl unmask openvpn-client@personal-tcp.service'
alias vpn-start='vpn-udp-start'
alias vpn-stop='vpn-udp-stop'
alias vpn-restart='vpn-udp-restart'
alias vpn-mask='vpn-udp-mask'
alias vpn-unmask='vpn-udp-unmask'
