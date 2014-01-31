[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

# fonts
xset +fp /usr/share/fonts/local
xset fp rehash
xsetroot -cursor_name left_ptr
setxkbmap -option ctrl:nocaps
/usr/lib/systemd/systemd --user &
exec xscreensaver -no-splash &
xrandr --output HDMI3 --primary
xrandr --output HDMI1 --right-of HDMI3
autocutsel -fork &
autocutsel -selection PRIMARY -fork &
[[ -f $HOME/.dropbox-dist/dropbox ]] && $HOME/.dropbox-dist/dropbox /newerversion &
exec xmonad
