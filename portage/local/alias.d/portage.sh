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

alias emerge-update='sudo emerge --update --keep-going --deep --with-bdeps\=n $@'
alias emerge-install='sudo emerge $@'
alias emerge-remove='sudo emerge --depclean $@'
alias emerge-remove-force='sudo emerge --unmerge $@'
alias emerge-clean='sudo emerge --with-bdeps\=n --depclean'
alias emerge-clear='sudo eclean -d -i distfiles'
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
alias emerge-status='(cd /var/db/repos/gentoo && exec git show -2 --summary)'

emerge-info() {
	equery m $@ 2> /dev/null
	echo ''
	eix -e $@ 2> /dev/null
	echo ''
	equery uses $@ 2> /dev/null
	echo ''
	echo 'Size:'
	equery size $@ 2> /dev/null || echo "Not installed"
}

emerge-reinstall() {
	sudo emerge --unmerge $@
	sudo emerge -1 $@
	# Ask whether add params to @world or not
	sudo emerge $@
}

emerge-pull() {
	local SUDO='sudo -E'

	if ! $SUDO true > /dev/null; then
		return $?
	fi

	(cd /var/db/repos/gentoo &&
		 $SUDO git checkout rsync &&
		 $SUDO git pull --force &&
		 $SUDO git reset --hard origin/rsync)
	(cd /var/db/repos/public &&
		 $SUDO git pull --force &&
		 $SUDO git reset --hard origin/master)
	(cd /var/db/repos/private &&
		 $SUDO git pull --force &&
		 $SUDO git reset --hard origin/master)
	wait
	$SUDO eix-update
}

emerge-sync() (
	local cdate=$(date +'%b %d, %Y')
	local SUDO='sudo -E'

	cd /var/db/repos/gentoo
	# Clean uncompleted syncs
	FILE=".tmp-unverified-download-quarantine"
	if [ -f "$FILE" ]; then
		$SUDO rm -R "$FILE"
		$SUDO git reset --hard
	fi

	# Update mirror branch by fetching commits from the official gentoo repo
	$SUDO git checkout rsync &&
		$SUDO git reset --hard origin/rsync; $SUDO git clean -d -f
	$SUDO git checkout master &&
		($SUDO git pull gentoo master &&
			 $SUDO chown $USER .git -R &&
			 git push origin master &&
			 $SUDO chown root .git -R)

	git status
	echo "Press Enter to continue"
	read enter

	# Update without-manifest branch, a master sub-branch that it does have
	# manifest files
	$SUDO git checkout without-manifest &&
		 ($SUDO git reset --hard origin/without-manifest
		  $SUDO git clean -d -f
		  $SUDO git merge master --no-edit
		  $SUDO find . -name 'Manifest*' -type f -delete
		  # Change the user ownership to the current user temporarily, so that GPG
		  # work correctly. Otherwise, GPG complains about TTY ownership when
		  # signing the commit and pushing it via ssh.
		  $SUDO chown $USER .git -R && git add --all
		  git commit -am "Merge master without manifest files $cdate" &&
			  git push origin without-manifest
		  $SUDO chown root .git -R
		  $SUDO git status)

	echo "Press Enter to continue"
	read enter

	emerge-rsync
)

emerge-rsync() (
	local cdate=$(date +'%b %d, %Y')
	local SUDO='sudo -E'

	cd /var/db/repos/gentoo

	$SUDO git checkout rsync
	$SUDO git reset --hard origin/rsync
	$SUDO git clean -d -f

	# Rsync and merge the new changes
	$SUDO git merge without-manifest --no-edit
	$SUDO emerge --sync | tee /tmp/rsync &&
		local remote=$(awk '/^rsync:/{gsub("?","",$1); print $1}' /tmp/rsync)

	# Change the user ownership to the current user temporarily, so that GPG
	# work correctly. Otherwise, GPG complains about TTY ownership when
	# signing the commit and pushing it via ssh.
	$SUDO chown $USER .git -R &&
		git add --all &&
		git commit -am "Merge $remote $cdate"

	git status
	echo "Press Enter to continue"
	read enter

	git push origin rsync
	$SUDO chown root .git -R
	$SUDO eix-update
)
