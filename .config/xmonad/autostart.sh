#!/usr/bin/env bash
BG=$HOME/.config/backgrounds

xsetroot -solid "#181818"
feh --bg-fill $BG/bottom-right.png $BG/bottom-left.png $BG/top-right.png $BG/top-left.png
killall picom
picom &
killall polybar
polybar -r top &
