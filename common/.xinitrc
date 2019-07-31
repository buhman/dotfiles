xrdb -load ~/.Xresources

setxkbmap -option ctrl:nocaps,compose:ralt
xmodmap ~/.Xmodmap
xsetroot -cursor_name left_ptr &

# DRI_PRIME
xrandr --setprovideroffloadsink 1 0

# touchpad acceleration
xinput set-prop 14 302 0.4

touchegg &

emacs --daemon &

echo 1 | sudo tee /sys/devices/system/cpu/intel_pstate/no_turbo

# https://github.com/flatpak/xdg-desktop-portal-gtk/issues/72
dbus-update-activation-environment --systemd DBUS_SESSION_BUS_ADDRESS DISPLAY XAUTHORITY

#export GTK_IM_MODULE=xim
#export XMODIFIERS=@im=ibus
#export QT_IM_MODULE=xim

exec xmonad
