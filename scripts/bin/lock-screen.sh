#!/bin/sh

# Take a screen shot
file=/tmp/screen.png
scrot "${file}"

# Rescale it to make it blurry
convert "${file}" -scale 15% -scale 667.5% "${file}"

# Lock the screen and put the modified image in background
i3lock -u -i "${file}" &
