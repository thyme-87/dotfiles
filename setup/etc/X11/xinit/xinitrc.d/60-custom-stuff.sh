#!/bin/sh
#FIXME this is not user specific but should be - as for other users the files won't be present/accessible
setxkbmap de
picom --conf ${HOME}/.picom.conf &
xsetroot -cursor_name left_ptr
${HOME}/.fehbg &
urxvt &
