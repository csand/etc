#!/bin/sh

# This exists because I hate theming the Gnome terminal

# ! Moria dark theme
# *xterm*background: #202020
# *xterm*foreground: #d0d0d0
# *xterm*cursorColor: #ffa500
# *xterm*color0: #101010
# *xterm*color1: #8b0000
# *xterm*color2: #008b00
# *xterm*color3: #786000
# *xterm*color4: #1f3f81
# *xterm*color5: #800090
# *xterm*color6: #008b8b
# *xterm*color7: #a0a0a0
# *xterm*color8: #404040
# *xterm*color9: #ee2c2c
# *xterm*color10: #87df71
# *xterm*color11: #EDBC80
# *xterm*color12: #7ec9ee
# *xterm*color13: #d7a0d7
# *xterm*color14: #7ee0ce
# *xterm*color15: #d0d0d0

profile="Default"

gconftool-2 --type string --set /apps/gnome-terminal/profiles/$profile/palette "\
#101010:#8b0000:#008b00:#786000:#1f3f81:#800090:#008b8b:#a0a0a0:#404040:#ee2c2c:#87df71:#edbc80:#7ec9ee:#d7a0d7:#7ee0ce:#d0d0d0\
"

# These seem to fuck up for some reason if the format isn't just right
gconftool-2 --type string --set /apps/gnome-terminal/profiles/$profile/background_color "202020202020"
gconftool-2 --type string --set /apps/gnome-terminal/profiles/$profile/foreground_color "D0D0D0D0D0D0"

