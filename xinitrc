#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# enable libva-vdpau-driver to enable hardware decoding for MPEG2 
# and (if possible) MPEG4 and H264
export LIBVA_DRIVER_NAME=vdpau

xsetroot -curser_name left_ptr

# exec gnome-session
# exec startkde
# exec startxfce4
# ...or the Window Manager of your choice
