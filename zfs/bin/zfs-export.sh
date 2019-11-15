#!/bin/bash

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

# A small script to export some zfs sets. The script is designed to be executed
# once a day.

if [ $UID -ne 0 ]; then
	echo "Please run this program as superuser"
	exit 1
fi

# Get the settings
. /home/$USERNAME/bin/settings.sh

# ZFS sets
backup_sets=('rpool/sys/root/default'
			 'rpool/data/yad/default'
			 'rpool/data/yad/notes'
			 'rpool/data/yad/archive'
			 'rpool/data/yad/music')

# GPG Key for backups
key=$(cat $GNUPGHOME/usage/zfs-backup.encrypt.key)

# Get the target location
if [ "$#" -eq 1 ]; then
	location="${1}/backup"
else
	location="/cloud/backup"
fi

function backup {

	# To avoid syncing to google drive while exporting the snapshot
	echo "Pause Insync"
	sudo -u "$USERNAME" insync-headless pause_syncing &> /dev/null

	backup_set=$1
	# Make sure the folder of the backup file exists
	mkdir -p "${location}/${backup_set}" 2> /dev/null

	if [ "$#" -eq 2 ]; then
		name=$2
		option='-R'
		prefix='full-'
	else
		name=$3
		option="-R -I ${backup_set}@$2"
		prefix="$2-to-"
	fi

	echo "Exporting ZFS Set ${backup_set} from Snapshot $2 to $name"
	echo "Using GPG key ${key}"

	zfs send ${option} ${backup_set}@${name} |
		lz4 |
		gpg --yes --batch --compress-algo none \
			--homedir "$GNUPGHOME" \
			-r "${key}!" \
			-o "${location}/${backup_set}/${prefix}${name}.lz4.gpg" \
			--encrypt

	echo "Resume Insync"
	sudo -u "$USERNAME" insync-headless resume_syncing &> /dev/null

}

# Compute necessary names
major_num=$(date +%Y-%m) #once a month
# Previous backup name
prev=$(date -d '1 day ago' +%Y-%m-%d-000000)
# Current backup name
new=$(date +%Y-%m-%d-000000)


for i in "${backup_sets[@]}"
do

	echo "Create a new major snap if it is required"
	#The following line will run once a month only
	zfs snap -r "${i}@${major_num}" 2> /dev/null &&
		backup "${i}" "${major_num}"

	echo "Check if we are out of sync"
	zfs snap -r "${i}@${prev}" 2> /dev/null &&
		backup "${i}" "${major_num}" "${prev}"

	#Create a zfs snapshot for the new backup if it does not exist
	zfs snap -r "${i}@${new}" 2> /dev/null

	case "$(date +%a)" in
		# If it is Monday, create a backup starting from the beginning of the month
		# instead. This is a safety feature to avoid data loss caused by corrupted
		# incremental backups
		Mon)
			backup ${i} "${major_num}" "${new}"
			;;
		*)
			# Perform an incremental backup
			 backup "${i}" "${prev}" "${new}"
			 ;;
	esac
done
