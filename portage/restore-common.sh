#!/bin/bash

cd `dirname $0`
echo "* Execute ${PWD}/`basename $0`"
. ../utils.sh

dst="/etc/portage/package.accept_keywords"
if [ -f "$dst" ]; then
	sudo rm "$dst"
fi

do-sync "$PWD/local/alias.d" "$HOME/.config/alias.d"
sudo-do-sync "$PWD/system/etc/portage/bashrc" "/etc/portage"
sudo-do-sync "$PWD/system/etc/portage/package.license/zz-required.license" "/etc/portage/package.license"
sudo-do-sync "$PWD/system/etc/portage/repos.conf" "/etc/portage/repos.conf"
