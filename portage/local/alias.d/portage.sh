#/bin/sh
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

alias emerge-update='sudo emerge --update --keep-going --deep --with-bdeps\=n $@'
alias emerge-install='sudo emerge $@'
alias emerge-remove='sudo emerge --depclean $@'
alias emerge-remove-force='sudo emerge --unmerge $@'
alias emerge-clean='sudo emerge --with-bdeps\=n --depclean'
alias emerge-search='emerge --search $@'
alias emerge-list='qlist -I'
alias emerge-has='equery hasuse $@'
alias emerge-dep='equery depends $@'
alias emerge-req='equery depgraph $@'
alias emerge-files='equery files $@'
alias emerge-belong='equery belongs $@'
alias emerge-time='sudo qlop -H $@'
alias emerge-log='sudo elogv'
alias emerge-fetch='sudo tail -f /var/log/emerge-fetch.log'

emerge-info () {
	equery m $@ 2> /dev/null
	echo ''
	eix -e $@ 2> /dev/null
	echo ''
	equery uses $@ 2> /dev/null
	echo ''
	echo 'Size:'
	equery size $@ 2> /dev/null || echo "Not installed"
}

emerge-reinstall () {
	sudo emerge --unmerge $@
	sudo emerge -1 $@
	# Ask whether add params to @world or not
	sudo emerge $@
}

emerge-sync () {
	local cdate=$(date +'%b %d, %Y')
	local SUDO='sudo -E'

	pushd . > /dev/null
	cd /var/db/repos/gentoo

	# Clean uncompleted syncs
	$SUDO rm -R .tmp-unverified-download-quarantine 2> /dev/null
	$SUDO git clean -fd

	# Update mirror branch by fetching commits from the official gentoo repo
	$SUDO git checkout master &&
		$SUDO git pull gentoo master
	$SUDO git push origin master

	# Update without-manifest branch, which is a master sub-branch but it does
	# not include manifest files
	$SUDO git checkout without-manifest &&
		$SUDO  git merge master --no-edit
	$SUDO find . -name 'Manifest' -delete
	$SUDO find . -name 'Manifest.gz' -delete
	$SUDO git commit -am "Merge master without manifest files $cdate"
	$SUDO git push origin without-manifest

	$SUDO git checkout rsync &&
		$SUDO -E git merge without-manifest --no-edit

	# Rsync and merge the new changes
	$SUDO sudo emerge --sync | tee /tmp/rsync &&
		local remote=$(awk '/^rsync:/{gsub("?","",$1); print $1}' /tmp/rsync) &&
		# @TODO This adds manifest files back to git. However, other files
		# may also be modified by rsync. We must merge everything, otherwise portage
		# is going to complain about manifest files.
		$SUDO git commit -am "Merge $remote $cdate"
	$SUDO git push origin rsync

	$SUDO eix-update
	popd > /dev/null
}
