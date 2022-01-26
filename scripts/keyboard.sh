#!/bin/bash

xset b 100 0 0
xset r rate 200 40
xbindkeys
setxkbmap -option compose:ralt # for kmonad?

kmonad $HOME/.config/kmonad/config.kbd &
kmonad $HOME/.config/kmonad/cornelius.kbd &

