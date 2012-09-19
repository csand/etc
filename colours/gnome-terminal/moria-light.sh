#!/bin/sh

# This exists because I hate theming the Gnome terminal

# ! Moria light theme
# *xterm*background: #f0f0f0
# *xterm*foreground: #000000
# *xterm*cursorColor: #883400
# *xterm*color0: #101010
# *xterm*color1: #8b0000
# *xterm*color2: #077807
# *xterm*color3: #786000
# *xterm*color4: #1f3f81
# *xterm*color5: #800090
# *xterm*color6: #007080
# *xterm*color7: #a0a0a0
# *xterm*color8: #404040
# *xterm*color9: #ee2c2c
# *xterm*color10: #90e090
# *xterm*color11: #ffcd78
# *xterm*color12: #7ec0ee
# *xterm*color13: #ee2cee
# *xterm*color14: #00ffff
# *xterm*color15: #d0d0d0

profile="Default"

gconftool-2 --type string --set /apps/gnome-terminal/profiles/$profile/palette "\
#101010:#8b0000:#077807:#786000:#1f3f81:#800090:#007080:#a0a0a0:#404040:#ee2c2c:#90e090:#ffcd78:#7ec0ee:#ee2cee:#00ffff:#d0d0d0\
"
gconftool-2 --type string --set /apps/gnome-terminal/profiles/$profile/background_color "f0f0f0"
gconftool-2 --type string --set /apps/gnome-terminal/profiles/$profile/foreground_color "000000"

