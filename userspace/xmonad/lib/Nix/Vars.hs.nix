{ scripts, alsaUtils, copyq, i3lock-fancy, xautolock
, xmobar, rofi, libqalculate, st, xmessage, dbus, tmux, rofi-scripts, xbacklight
}:
''
module Nix.Vars where

dbusSend = "${dbus}/bin/dbus-send"
st = "${st}/bin/st"
amixer = "${alsaUtils}/bin/amixer"
copyQ = "${copyq}/bin/copyq"
i3Lock = "${i3lock-fancy}/bin/i3lock-fancy"
xautolock = "${xautolock}/bin/xautolock"
xmobar = "${xmobar}/bin/xmobar"
rofi = "${rofi}/bin/rofi"
rofiScripts = "${rofi-scripts}/bin/scripts"
qalc = "${libqalculate}/bin/qalc"
xmessage = "${xmessage}/bin/xmessage"
tmux = "${tmux}/bin/tmux"
xbacklight = "${xbacklight}/bin/xbacklight"

randomBgScript = "${scripts.randomBackground}"
lockptrScript = "${scripts.lockptr}"
printVolScript = "${scripts.printVol}"
printVolLvlScript = "${scripts.printVolLvl}"
wirelessScript = "${scripts.wireless}"
''
