#!/bin/bash


# General
export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -c"          # $EDITOR opens in terminal
export VISUAL="emacsclient -c"         # $VISUAL opens in GUI mode
export TERMINAL="gnome-terminal"
export BROWSER="firefox-developer-edition"
export JAVA_HOME="/usr/lib/jvm/default"

# mpd
export MPD_HOST="localhost"
export MPD_PORT="6600"

# gpg & ssh
export GNUPGHOME=/home/yad/.config/gpg
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)

# Terminal
export RXVT_SOCKET=/run/user/1000/urxvtd-arch-laptop

# Colors
export PANEL_FONT_FAMILY="DejaVu Sans Mono-6"
export PANEL_FONT_FAMILY2="FontAwesome-7"

export PANEL_FIFO="/tmp/lemon-panel-fifo"
export PANEL_HEIGHT="35"
export PANEL_WM_NAME=lemon_bspwm_panel
export COLOR_FOREGROUND='#e6beae'
export COLOR_BACKGROUND='#0f0b0c'
export COLOR_ACTIVE="#392626"
export COLOR_ACTIVE2="#bd9c8f"
export COLOR_INDICATOR1="#a7e23e"
export COLOR_INDICATOR2="#6fd8ff"
export COLOR_INDICATOR3="#ffa500"
export COLOR_INDICATOR4="#ff4422"

export COLOR_TITLE_FG=$COLOR_FOREGROUND
export COLOR_TITLE_BG=$COLOR_BACKGROUND
export COLOR_MONITOR_FG=$COLOR_FOREGROUND
export COLOR_MONITOR_BG=$COLOR_BACKGROUND
export COLOR_MONITOR_ACTIVE_FG=$COLOR_FOREGROUND
export COLOR_MONITOR_ACTIVE_BG=$COLOR_BACKGROUND
export COLOR_MONITOR_FOCUSED_FG=$COLOR_INDICATOR3
export COLOR_MONITOR_FOCUSED_BG=$COLOR_BACKGROUND
export COLOR_DESKTOP_FG=$COLOR_FOREGROUND
export COLOR_DESKTOP_BG=$COLOR_BACKGROUND
export COLOR_DESKTOP_ACTIVE_FG=$COLOR_FOREGROUND
export COLOR_DESKTOP_ACTIVE_BG=$COLOR_ACTIVE
export COLOR_DESKTOP_FOCUSED_FG=$COLOR_BACKGROUND
export COLOR_DESKTOP_FOCUSED_BG=$COLOR_INDICATOR1
export COLOR_DESKTOP_URGENT_FG=$COLOR_BACKGROUND
export COLOR_DESKTOP_URGENT_BG=$COLOR_INDICATOR3


