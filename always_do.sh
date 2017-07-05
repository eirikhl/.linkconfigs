#!/bin/bash

# Pull the dotfiles repo
cd ~/.linkconfigs && git pull --rebase

# Set Caps Lock to Ctrl
setxkbmap -option ctrl:nocaps

# Activate NumLock
cd / && ./usr/bin/numlockx on
