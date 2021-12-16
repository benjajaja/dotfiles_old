# scale output together with DPI of 192 in .Xresources
xrandr --output eDP1 --scale 1.25x1.25
xrdb -merge ~/.Xresources
#export GDK_SCALE=2
gsettings set org.gnome.desktop.interface text-scaling-factor 2
# background
hsetroot -center $HOME/Pictures/skull_on_fire_framed_c700-477x480.jpg
# composite for Optimus GPU offloading (games)
xcompmgr -a &

# tray
nm-applet &
pa-applet &
xfce4-clipman &
cbatticon &
#gtrayicon --activate=\"xinput float $(keyboardid)\" --deactivate=\"xinput reattach $(keyboardid) 3\" &
# /usr/bin/trayer --edge top --align right --SetPartialStrut true --transparent true --tint 0x000000 -l --height 32 --iconspacing 4 &

# screensaver
xscreensaver &
xss-lock xscreensaver-command &

# keyboard
xset b 100 0 0
xset r rate 200 40
xbindkeys
kmonad $HOME/.config/kmonad/config.kbd &
kmonad $HOME/.config/kmonad/cornelius.kbd &

xmonad
