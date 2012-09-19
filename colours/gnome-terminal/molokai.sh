#!/bin/sh

# This exists because I hate theming the Gnome terminal

# Original Xdefaults, from https://gist.github.com/845878
#
# ! Molokai theme
# *xterm*background: #101010
# *xterm*foreground: #d0d0d0
# *xterm*cursorColor: #d0d0d0
# *xterm*color0: #101010
# *xterm*color1: #960050
# *xterm*color2: #66aa11
# *xterm*color3: #c47f2c
# *xterm*color4: #30309b
# *xterm*color5: #7e40a5
# *xterm*color6: #3579a8
# *xterm*color7: #9999aa
# *xterm*color8: #303030
# *xterm*color9: #ff0090
# *xterm*color10: #80ff00
# *xterm*color11: #ffba68
# *xterm*color12: #5f5fee
# *xterm*color13: #bb88dd
# *xterm*color14: #4eb4fa
# *xterm*color15: #d0d0d0

profile="Default"

gconftool-2 --type string --set /apps/gnome-terminal/profiles/$profile/palette "\
#101010:#960050:#66AA11:#C47F2C:#30309B:#7E40A5:#3579A8:#9999AA:#303030:#FF0090:#80FF00:#FFBA68:#5F5FEE:#BB88DD:#4EB4FA:#D0D0D0\
"
gconftool-2 --type string --set /apps/gnome-terminal/profiles/$profile/background_color "#101010"
gconftool-2 --type string --set /apps/gnome-terminal/profiles/$profile/foreground_color "#D0D0D0"
