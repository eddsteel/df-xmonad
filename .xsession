#!/bin/bash

export WINDOW_MANAGER=xmonad
if [ -f /home/edd/bin/autostart.sh ]; then
	/home/edd/bin/autostart.sh &
fi
exec /usr/bin/gnome-session
