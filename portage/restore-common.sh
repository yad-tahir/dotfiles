#!/bin/bash

cd `dirname $0`
echo "* Execute ${PWD}/`basename $0`"
. ../utils.sh

[ ! -e "/etc/portage/repos.conf" ] && sudo mkdir --parents "/etc/portage/repos.conf"

[ ! -d "/etc/portage/package.accept_keywords" ] &&
	sudo rm /etc/portage/package.accept_keywords &&
	sudo mkdir /etc/portage/package.accept_keywords

do-ln-sync "$PWD/local/alias.d" "$HOME/.config/alias.d"

arr=( "/etc/portage/bashrc"
	  "/etc/portage/repos.conf/public.conf"
	  "/etc/portage/repos.conf/private.conf"
	  "/etc/portage/package.license/zz-required.license"
	)

for i in "${arr[@]}"
do
	TARGET=$i
	SOURCE=${PWD}/system${TARGET}
	do-sync-sudo "$SOURCE" "$TARGET"
done
