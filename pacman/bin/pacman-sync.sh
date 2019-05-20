#!/bin/sh

. /home/$USERNAME/bin/settings.sh

/bin/rm /var/lib/pacman/db.lck 2> /dev/null;
pacman -Syy

#Notify lemon-bar
sudo -u $USERNAME /bin/sh -c "/home/$USERNAME/.config/lemon-bar/blocks/packages.sh > $PANEL_FIFO"
