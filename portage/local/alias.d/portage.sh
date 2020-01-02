#/bin/sh
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

alias emerge-update='sudo emerge --update --deep --keep-going --with-bdeps\=y $@'
alias emerge-install='sudo emerge $@'
alias emerge-remove='sudo emerge --depclean $@'
alias emerge-remove-force='sudo emerge --unmerge $@'
alias emerge-clean='sudo emerge --depclean'
alias emerge-list='qlist -I'
alias emerge-has='equery hasuse $@'
alias emerge-dep='equery depends $@'
alias emerge-req='equery depgraph $@'
alias emerge-files='equery files $@'
alias emerge-belong='equery belongs $@'
alias emerge-time='sudo qlop $@'
alias emerge-log='sudo elogv'
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
	# Ask whether add it to @world or not
	sudo emerge $@
}
emerge-sync () {
	pushd .
	local cdate=$(date +'%b %d, %Y')
	cd /var/db/repos/gentoo
	sudo rm -R .tmp-unverified-download-quarantine 2> /dev/null
	sudo git clean -fd
	# Update mirror branch by fetching commits from the official gentoo repo
	sudo -E git checkout master &&
		sudo -E git reset --hard origin/master &&
		sudo -E git pull gentoo master
	sudo -E git push origin master

	# Update without-manifest branch, which is a mirror sub-branch but without manifest files
	sudo -E git checkout without-manifest &&
		sudo -E git merge master
		sudo -E find . -name 'Manifest' -delete
		sudo -E find . -name 'Manifest.gz' -delete
		sudo -E git commit -am "Merge master without manifest files $cdate"
	sudo -E git push origin without-manifest

	sudo -E git checkout rsync &&
		sudo -E git merge without-manifest
		# Rsync and merge the new changes
		sudo -E sudo emerge --sync | tee /tmp/rsync &&
		local remote=$(cat /tmp/rsync | awk '/^rsync:/{gsub("?","",$1); print $1}') &&
		# @TODO This adds manifest files back to master. However, other files
		# may also be modified by rsync. We must merge everything, otherwise portage
		# is going to complain about manifest files.
		sudo -E git add . &&
		sudo -E git commit -m "Merge $remote $cdate"
	sudo -E git push origin rsync

	sudo eix-update
	popd
}
