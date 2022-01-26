# nvidia laptop gfx mode switch
/usr/bin/prime-offload

# scale output together with DPI of 192 in .Xresources
xrandr --output eDP1 --scale 1.25x1.25
xrdb -merge ~/.Xresources
#export GDK_SCALE=2
gsettings set org.gnome.desktop.interface text-scaling-factor 2

# background
hsetroot -center $HOME/Pictures/skull_on_fire_framed_c700-477x480.jpg

# notifications
dunst &

# tray (spawned from xmonad.hs now)
# /usr/bin/trayer --edge top --align right --SetPartialStrut true --transparent true --tint 0x000000 -l --height 32 --iconspacing 4 &

# tray apps
nm-applet &
pa-applet &
xfce4-clipman &
cbatticon &
optimus-manager-qt &
flameshot &
mictray &
#gtrayicon --activate=\"xinput float $(keyboardid)\" --deactivate=\"xinput reattach $(keyboardid) 3\" &

# screensaver
xscreensaver &
xss-lock xscreensaver-command &

# keyboard
bash $HOME/scripts/keyboard.sh

xmonad

# nvidia laptop gfx mode switch
sudo /usr/bin/prime-switch
