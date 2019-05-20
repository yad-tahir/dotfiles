#
# /etc/bash.bashrc
#

. ~/.profile

# if not running interactively, don't do anything
[[ $- != *i* ]] && return

# double check window size
[[ $DISPLAY ]] && shopt -s checkwinsize

# start bash completion
[ -r /usr/share/bash-completion/bash_completion   ] && . /usr/share/bash-completion/bash_completion


# welcome screen
# neofetch
systemctl list-units -t service --failed --quiet --no-pager
