#!/bin/sh

# This exists because I hate theming the Gnome terminal

# Original Xdefaults from http://onethingwell.org/post/6002086162/colours
#
# *background: rgb:F5/F5/F5
# *foreground: rgb:44/44/44
# *color0:  rgb:00/00/00
# *color1:  rgb:9e/18/28
# *color2:  rgb:5c/b2/47
# *color3:  rgb:96/8a/38
# *color4:  rgb:41/61/a0
# *color5:  rgb:9b/76/8e
# *color6:  rgb:41/91/89
# *color7:  rgb:ee/ea/ea
# *color8:  rgb:66/66/66
# *color9:  rgb:cf/61/71
# *color10: rgb:c5/a7/79
# *color11: rgb:ff/f7/96
# *color12: rgb:41/86/be
# *color13: rgb:cf/9e/be
# *color14: rgb:71/be/be
# *color15: rgb:dd/dd/dd

profile="Default"

gconftool-2 --type string --set /apps/gnome-terminal/profiles/$profile/palette "\
#000000:#9E1828:#5CB247:#968A38:#4161A0:#9B768E:#419189:#EEEAEA:#666666:#CF6171:#C5A779:#FFF796:#4186BE:#CF9EBE:#71BEBE:#DDDDDD\
"
gconftool-2 --type string --set /apps/gnome-terminal/profiles/$profile/background_color "#F5F5F5"
gconftool-2 --type string --set /apps/gnome-terminal/profiles/$profile/foreground_color "#444444"
