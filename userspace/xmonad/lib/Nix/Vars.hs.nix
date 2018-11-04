{ dotfilesLoc, alsaUtils, copyq, i3lock-fancy, xmonad, xautolock, xmobar, rofi, libqalculate, st }:
''
module Nix.Vars where

dotfilesLocation = "${dotfilesLoc}"

st = "${st}/bin/st"
xmonad = "${xmonad}/bin/xmonad"
amixer = "${alsaUtils}/bin/amixer"
copyQ = "${copyq}/bin/copyq"
i3Lock = "${i3lock-fancy}/bin/i3lock-fancy"
xautolock = "${xautolock}/bin/xautolock"
xmobar = "${xmobar}/bin/xmobar"
rofi = "${rofi}/bin/rofi"
qalc = "${libqalculate}/bin/qalc"
''
