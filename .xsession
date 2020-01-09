~/.bin/displayfix

xmodmap .xmodmap

xset b 100 0 0
xset r rate 200 40
setxkbmap -option caps:escape

#trayer --edge top --align right --SetPartialStrut true --transparent true --tint 0x000000 -l &

# stalonetray -t --icon-gravity NW -bg "#000000" &
nm-applet &
xfce4-clipman &
hsetroot -center /home/bgrosse/Pictures/skull_on_fire_framed_c700-477x480.jpg

# xmonad
# startxfce4
#dbus-launch --exit-with-session xmonad
