#!/bin/bash
background="#111111"
foreground="#D0E0E4"
myFont="Hack:style=Regular::size=9"
let "endofLeftBar=1000"

#Grabbing the resolution of current monitor (must have xrandr installed)
resX=$(xrandr | grep current | sed -e 's/.*current \(.*\),.*/\1/' -e 's/ x [0-9]*//') #For Desktop monitor this is 2560 
resY=$(xrandr | grep current | sed -e 's/.*current \(.*\),.*/\1/' -e 's/[0-9]* x //') #For Desktop monitor this should be 1440
let "startingPosition=$endofLeftBar+1"
let "width=$resX-$startingPosition"
dzen2 -fn $myFont -bg $background -fg $foreground -x $startingPosition -y '0' -w $width -ta 'l'
