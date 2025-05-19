#!/usr/bin/env bash
BG=$HOME/.config/backgrounds

xsetroot -solid "#181818"
feh --bg-fill $BG/bottom-right.png $BG/bottom-left.png $BG/top-right.png $BG/top-left.png
## picom tends to not wake up properly from a lid close,
## xcompmgr is a suitable replacement
#pkill picom
#picom &
pkill xcompmgr
xcompmgr -cfF &
pkill polybar
polybar -r top &
