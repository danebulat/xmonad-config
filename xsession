#!/usr/bin/bash

# DESCRIPTION
# A sample .xsession configuration file. Save this file in your
# home directory and rename it to .xsession. The commands in this
# file will be executed before xmonad is started. Add custom
# commands to customise your own startup environment.

# Start compositor
picom -cb

# Restore wallpaper
nitrogen --restore

# Monitor configuration
xrandr --output HDMI-2 --mode 1920x1080
xrandr --output eDP-1 --mode 1920x1080 --right-of HDMI-2

# Set up an icon tray on 2 monitors
trayer --edge bottom --align right --SetDockType true --SetPartialStrut true --expand true \
       --width 3 --transparent true --tint 0x0a0a0a --height 21 --monitor 1 --alpha 0 --padding 2 &

trayer --edge bottom --align right --SetDockType true --SetPartialStrut true --expand true \
       --width 3 --transparent true --tint 0x0a0a0a --height 21 --monitor 0 --alpha 0 --padding 2 &

# Battery tray icon
xfce4-power-manager &

# Volume tray icon
volumeicon &

# Network tray icon
if [ -x /usr/bin/nm-applet ] ; then
    nm-applet --sm-disable &
fi

# Spawn xmobar on 2nd monitor
xmobar -x 1 ~/.config/xmobar/xmobarrc &

# Start xmonad
exec xmonad
