#!/bin/bash
TMPBG=/tmp/screen.png
scrot /tmp/screen.png
convert $TMPBG -scale 15% -scale 667.5% $TMPBG
i3lock -u -i $TMPBG
