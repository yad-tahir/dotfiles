#! /bin/sh
#

PWD=$(dirname $0)

. $HOME/bin/settings.sh

# Kill any panel processes older than us, instead of bailing like the example
# does. That caused one too many panel-less boots for me.

while [ $(pgrep -cx panel.sh) -gt 1 ] ; do
	pkill -ox -9 panel.sh
done

# Kill any remaining trays / xtitle instances so we don't have multiples.
killall -9 bspc 2> /dev/null
killall -9 lemonbar 2> /dev/null
# Remove the old panel completely
xdo id -a "$PANEL_WM_NAME" | xargs -n 1 -I % xdo kill %

# Setup taken from example, tell bspwm to avoid our status/tray and to start
# sending status updates to a FIFO
trap 'trap - TERM; kill 0' INT TERM QUIT EXIT

[ -e "$PANEL_FIFO" ] && rm "$PANEL_FIFO"
mkfifo "$PANEL_FIFO" -m600

bspc config top_padding $PANEL_HEIGHT
bspc subscribe report > "$PANEL_FIFO" &

# Here are the subprograms that add information to the status FIFO which are
# interpreted by panel_bar, below. Each output is detected by its first
# character, which is how the bspwm internal information is presented.

killall -9 xtitle 2> /dev/null
xtitle -sf 'T%s\n' > "$PANEL_FIFO" &

# Sys Blocks
# Here are the blocks that must be refreshed regularly. The refresh
# delay, in seconds, is passed as a first argument to each script.
. $PWD/blocks/clock.sh 1 > "$PANEL_FIFO" &
. $PWD/blocks/temperature.sh 10 > "$PANEL_FIFO" &
. $PWD/blocks/fan.sh 10 > "$PANEL_FIFO" &
. $PWD/blocks/memory.sh 20 > "$PANEL_FIFO" &
. $PWD/blocks/processes.sh 20 > "$PANEL_FIFO" &
. $PWD/blocks/battery.sh 20 > "$PANEL_FIFO" &
. $PWD/blocks/net-speed.sh 60 > "$PANEL_FIFO" &
. $PWD/blocks/sync.sh 120 > "$PANEL_FIFO" &
# Event-driven blocks
. $PWD/blocks/music.sh > "$PANEL_FIFO"&
. $PWD/blocks/volume.sh > "$PANEL_FIFO"&
. $PWD/blocks/keyboard.sh > "$PANEL_FIFO"&
. $PWD/blocks/caps.sh > "$PANEL_FIFO"&
. $PWD/blocks/bugs.sh > "$PANEL_FIFO" &
. $PWD/blocks/emacs-stats.sh > "$PANEL_FIFO" &
. $PWD/blocks/packages.sh > "$PANEL_FIFO"&
. $PWD/blocks/public-ip.sh > "$PANEL_FIFO"&

# Now panel_bar, which was mostly taken from the example panel_bar, with a
# handful of improvements.

num_mon=$(bspc query -M | wc -l)
function panel_bar {
	monitor_info_arr=("" "" "" "" "")
	titles=("" "" "" "" "")
	focused_wm_flags=("" "" "" "" "")
	while read line ; do
		case $line in
			# Handle Sys blocks
			S*)
				# sys output
				type=${line:1:1}
				lineCode=${line#??}
				case $type in
					f*)
						fan="$lineCode"
						;;
					p*)
						processes="$lineCode"
						;;
					m*)
						memory="$lineCode"
						;;
					e*)
						emacs="$lineCode"
						;;
					w*)
						music="$lineCode"
						;;
					b*)
						bugs="$lineCode"
						;;
					g*)
						sync="$lineCode"
						;;
					i*)
						net="$lineCode"
						;;
					n*)
						public_ip="$lineCode"
						;;
					j*)
						packages="$lineCode"
						;;
					t*)
						temperature="$lineCode"
						;;
					x*)
						battery="$lineCode"
						;;
					v*)
						volume="$lineCode"
						;;
					y*)
						caps="$lineCode"
						;;
					k*)
						keyboard="$lineCode"
						;;
					c*)
						clock=$lineCode
						;;
				esac
				sys="${music} ${caps} ${temperature} ${fan} | ${memory} ${processes} ${bugs} ${emacs} | ${packages} | ${sync}${net} ${public_ip} | ${battery} ${volume} ${keyboard} ${clock} "
				;;

			# Title
			T*)

				title="${line#?}"

				# Print the title along with flags only when it is not empty
				if [ "${title}" != "" ] ; then
					# Address long if the title is too long
					# title=$(printf "%0.100s" "${title}")

					# Before printing the title, check the node
					# flags and adjust the color according.

					flags="${focused_wm_flags[$selected_monitor]}"
					locked_flag=
					case "$flags" in *L*) locked_flag="L";;esac
					fixed_flag=
					case "$flags" in *P*) fixed_flag="F";;esac
					marked_flag=
					case "$flags" in *M*) marked_flag="M";;esac
					other_flag=


					## Set the dominated color. The priority order is
					## lock, fixed, marked and then other.
					if [ "$locked_flag" == "L" ] ; then
						FG=$COLOR_FOREGROUND
						UG=$COLOR_INDICATOR4
						BG=$COLOR_BACKGROUND
					elif [ "$fixed_flag" == "F" ] ; then
						FG=$COLOR_FOREGROUND
						UG=$COLOR_INDICATOR3
						BG=$COLOR_BACKGROUND
					elif [ "$marked_flag" == "M" ] ; then
						FG=$COLOR_FOREGROUND
						UG=$COLOR_INDICATOR2
						BG=$COLOR_BACKGROUND
					else
						FG=$COLOR_FOREGROUND
						UG=$COLOR_INDICATOR1
						BG=$COLOR_BACKGROUND
						other_flag="N${flags}"
					fi

					pid=$(xdo pid 2> /dev/null)
					title_text="${locked_flag}${fixed_flag}${marked_flag}${other_flag} ${pid} | "
					w_class=$(xprop -id $(xdo id) | awk '/^WM_CLASS/{print $4}' | sed 's/"//g')
					if [ "$w_class" = "Emacs" ]; then
						title_text="${title_text}${title:0:${#title}-7} "
						eposition="${title:(-4)}"
						eposition="${eposition:0:${#eposition}-1}"
						t="%{U$UG}%{F$FG}%{B$BG}"
						t="${t}"$($PWD/blocks/progress.sh " $title_text" $eposition)
						titles[$selected_monitor]="${t}%{B-}%{F-}%{U-}"

					else
						title_text="${title_text}${title}"
						titles[$selected_monitor]="%{U$UG}%{F$FG}%{B$BG}%{+u} ${title_text} %{-u}%{B-}%{F-}%{U-}"
					fi

				else
					titles[$selected_monitor]=
				fi
				;;
			W*)
				# bspwm internal state
				IFS=':'
				current_monitor=-1
				selected_monitor=0
				set -- ${line#?}
				while [ $# -gt 0 ] ; do
					item=$1
					name=${item#?}
					# echo $item
					case $item in
						[mM]*)
							case $item in
								m*)
									# monitor
									FG=$COLOR_MONITOR_FG
									BG=$COLOR_MONITOR_BG
									on_focused_monitor=
									;;
								M*)
									# focused monitor
									FG=$COLOR_MONITOR_FOCUSED_FG
									BG=$COLOR_MONITOR_FOCUSED_BG
									on_focused_monitor=1
									selected_monitor=$(($current_monitor+1))
									;;
							esac
							[ $num_mon -lt 2 ] && shift && continue
							# wm="${wm}%{-S${mon}}"
							current_monitor=$((current_monitor+1))
							monitor_info_arr[$current_monitor]=""
							# wm="${wm}%{S${current_monitor}}"
							monitor_info_arr[$current_monitor]="${wm_info_arry[$current_monitor]}%{F${FG}}%{B${BG}}%{A:bspc monitor -f ${name}:} Monitor ${name} %{A}%{B-}%{F-}"
							# wm="${wm}%{B${COLOR_BACKGROUND}} %{B-}"
							;;
						[fFoOuUG]*)
							case $item in
								f*)
									# free desktop
									FG=$COLOR_DESKTOP_FG
									BG=$COLOR_DESKTOP_BG
									UL=$BG
									;;
								F*)
									# free desktop
									if [ "$on_focused_monitor" ] ; then
										# focused occupied desktop
										FG=$COLOR_DESKTOP_FOCUSED_FG
										BG=$COLOR_DESKTOP_FOCUSED_BG
										UL=$BG
									else
										# active occupied desktop
										FG=$COLOR_DESKTOP_URGENT_BG
										BG=$COLOR_DESKTOP_ACTIVE_BG
										UL=$BG
									fi
									;;
								o*)
									# occupied desktop
									FG=$COLOR_DESKTOP_ACTIVE_FG
									BG=$COLOR_DESKTOP_ACTIVE_BG
									UL=$BG
									;;
								O*)
									if [ "$on_focused_monitor" ] ; then
										# focused occupied desktop
										FG=$COLOR_DESKTOP_FOCUSED_FG
										BG=$COLOR_DESKTOP_FOCUSED_BG
										UL=$BG
									else
										# active occupied desktop
										FG=$COLOR_DESKTOP_ACTIVE_FG
										BG=$COLOR_DESKTOP_ACTIVE_BG
										UL=$BG
									fi
									;;
								[uU]*)
									# urgent desktop
									FG=$COLOR_DESKTOP_URGENT_FG
									BG=$COLOR_DESKTOP_URGENT_BG
									UL=$BG
									;;
								G*)
									focused_wm_flags[$current_monitor]="${item#?}"
									name=
									;;
							esac
							if [ "$name" != "" ] ; then
								monitor_info_arr[$current_monitor]="${monitor_info_arr[$current_monitor]}%{F${FG}}%{B${BG}}%{A:bspc desktop -f ${name}:} ${name} %{A}%{B-}%{F-}"
							fi
							;;
					esac
					shift
				done
				;;
		esac

		#Create a panel for each monitor
		fmt=
		for (( i=0; i < $num_mon; i++ ))
		do
			fmt="${fmt}%{l}%{S$i}${monitor_info_arr[$i]} ${titles[$i]}%{r}${sys}"
		done

		printf "%s\n" "$fmt"

	done < $PANEL_FIFO
}
# panel_bar
# Actually invoking the panel and piping to bar
panel_bar | lemonbar -a 80 -p -g x$PANEL_HEIGHT -f "$PANEL_FONT_FAMILY" -f "$PANEL_FONT_FAMILY2" -F "$COLOR_FOREGROUND" -B "$COLOR_BACKGROUND" -u 2 -U "$COLOR_INDICATOR2" -n $PANEL_WM_NAME | /usr/bin/xargs -n 1 -I % /bin/sh -c % &

#killall -9 stalonetray 2> /dev/null
# stalonetray --geometry $TRAY_GEOM -i $PANEL_HEIGHT -bg "#${COLOR_BACKGROUND:3}" --grow-gravity NE --kludges force_icons_size &



xdo id -m -a "$PANEL_WM_NAME" | xargs -n 1 -I % xdo above -t "$(xdo id -N Bspwm -n root | sort | head -n 1)" %


wait
