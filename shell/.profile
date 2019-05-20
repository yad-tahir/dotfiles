# Place environment settings, since it gets executed
# automatically by the DisplayManager during the start-up process
# desktop session as well as by the login shell when one logs in from
# the textual console


# Shortcut commands
alias ls='ls -ali'
alias s="sudo su"
alias e="emacs"
alias ec="emacsclient -cn"
alias firefox="firefox-developer-edition"
alias pacman-history="expac --timefmt='%Y-%m-%d %T' '%l\t%n' | sort | tail -n 100"
alias pacman-unused='pacman -Qtdq'
alias kweb='sudo -u yad -g no_net KeeWeb'
alias cleaor='clear'

# Other variables
source ~/bin/settings.sh

# Adjust the path
PATH="~/bin:${PATH}"