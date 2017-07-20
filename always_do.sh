#!/bin/bash

# Pull the dotfiles repo
cd ~/.linkconfigs && git pull --rebase

# Set Caps Lock to Ctrl
setxkbmap -option ctrl:nocaps
# Fix key bindings, swap Caps Lock and Escape
xmodmap -e "keycode 9 = Caps_Lock NoSymbol Caps_Lock"
xmodmap -e "keycode 66 = Escape NoSymbol Escape"

# Activate NumLock
cd / && ./usr/bin/numlockx on
