if [ -n "$DISPLAY" ]; then
    if xmodmap -pke | awk '$3=="=" && $4=="q" {q=$2}
			 $3=="=" && $4=="w" {w=$2}
			 END {exit w-q==1}'; then
	setxkbmap us
	setxkbmap -option
    else
	setxkbmap dvorak
	setxkbmap -option altwin:swap_alt_win
    fi
elif [ "$TERM" = "linux" ]; then
    if dumpkeys | awk '$3=="=" && $4=="q" {q=$2}
		     $3=="=" && $4=="w" {w=$2}
		     END {exit w-q==1}'; then
	loadkeys us
    else
	loadkeys dvorak
    fi
fi

# notify i3blocks
pkill -RTMIN+24 i3blocks
