#!/bin/bash

# Pull the dotfiles repo
cd ~/.linkconfigs && git pull --rebase

# Activate NumLock
cd / && ./usr/bin/numlockx on
