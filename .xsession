~/.fehbg
xmodmap ~/.Xmodmap
xcape -e 'Shift_L=parenleft'
xcape -e 'Shift_R=parenright'

xcape -e 'Control_L=Escape'
xcape -e '0x1234=Return'

eval `dbus-launch --auto-syntax`
eval `gpg-agent --daemon`

xcompmgr &
taffybar &

xsetroot -cursor_name left_ptr

# bluetooth power on
dbus-send --system --type=method_call --print-reply=literal --dest=org.bluez /org/bluez/hci0 org.freedesktop.DBus.Properties.Set string:org.bluez.Adapter1 string:Powered variant:boolean:true

exec xmonad
