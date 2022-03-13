#!/bin/bash
# kmonad $HOME/.config/kmonad/config.kbd 2>/dev/null &

# sleep 1

xset b 100 0 0
xset r rate 200 40
xbindkeys
setxkbmap -option compose:ralt # for kmonad?


kmonad $HOME/.config/kmonad/cornelius.kbd
