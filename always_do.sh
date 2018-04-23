#!/bin/bash

# Norwegian Dvorak as default
setxkbmap no -variant dvorak
# Fix key bindings, swap Caps Lock and Escape
setxkbmap -option kpdl:dot
setxkbmap -option ctrl:nocaps
xmodmap -e "keycode 9 = Caps_Lock NoSymbol Caps_Lock"
xmodmap -e "keycode 66 = Escape NoSymbol Escape"
# Keyboard widget thing
exec ibus-daemon
# Activate NumLock
numlockx on

# Start Dropbox, because I always forget to
exec dropbox start

# Pull the dotfiles repo
cd ~/.linkconfigs && git pull --rebase

# Make it possible to run the lockscreen
exec xscreensaver &
