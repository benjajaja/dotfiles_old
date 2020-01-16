~/.bin/displayfix

# scale output together with DPI of 192 in .Xresources
xrandr --output eDP1 --scale 1.5x1.5

# unused?
xmodmap .xmodmap
# repeat
xset b 100 0 0
xset r rate 200 40
# special keys
setxkbmap -option caps:escape
setxkbmap -option altwin:swap_alt_win

# media keys
xbindkeys

# tray icons
nm-applet &
xfce4-clipman &
volumeicon &

# screensaver and lock
xscreensaver &
xss-lock -- xscreensaver-command -lock &

# desktop background
hsetroot -center $HOME/Pictures/skull_on_fire_framed_c700-477x480.jpg

# start tray with delay so it can go behind xmobar
( sleep 1 ; trayer --edge top --align right --SetPartialStrut true --transparent true --tint 0x000000 -l ) &
