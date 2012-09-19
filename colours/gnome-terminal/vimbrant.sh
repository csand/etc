#!/bin/sh

# Thayer's terminal colors

# ! terminal colours
# *foreground:#a0a0a0
# *background:#1b1d1e
# !black
# *color0:    #1b1d1e
# *color8:    #505354
# !red
# *color1:    #f92672
# *color9:    #ff5995
# !green
# *color2:    #82b414
# *color10:   #b6e354
# !yellow
# *color3:    #fd971f
# *color11:   #feed6c
# !blue
# *color4:    #56c2d6
# *color12:   #8cedff
# !magenta
# *color5:    #8c54fe
# *color13:   #9e6ffe
# !cyan
# *color6:    #465457
# *color14:   #899ca1
# *color6:    #56676b
# *color14:   #899ca1
# !white
# *color7:    #ccccc6
# *color15:   #f8f8f2
# *color7:    #bfbfba
# *color15:   #ebebe5
# *color7:    #aaaaaa
# *color15:   #ffffff

GTPROFILE="Default"

gconftool-2 --type string --set /apps/gnome-terminal/profiles/$GTPROFILE/palette "\
#1B1D1E:#F92672:#82B414:#FD971F:\
#56C2D6:#8C54FE:#56676B:#BFBFBA:\
#505354:#FF5995:#B6E354:#FEED6C:\
#8CEDFF:#9E6FFE:#899CA1:#EBEBE5"

gconftool-2 --type string --set /apps/gnome-terminal/profiles/$GTPROFILE/background_color "#1B1D1E"

gconftool-2 --type string --set /apps/gnome-terminal/profiles/$GTPROFILE/foreground_color "#A0A0A0"
