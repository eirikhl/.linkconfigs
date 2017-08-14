#!/bin/bash

# Pull the dotfiles repo
cd ~/.linkconfigs && git pull --rebase

# Norwegian Dvorak as default
setxkbmap no -variant dvorak
# Fix key bindings, swap Caps Lock and Escape
setxkbmap -option ctrl:nocaps
xmodmap -e "keycode 9 = Caps_Lock NoSymbol Caps_Lock"
xmodmap -e "keycode 66 = Escape NoSymbol Escape"

exec xscreensaver &

# Activate NumLock
cd / && ./usr/bin/numlockx on
