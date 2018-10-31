{ dotfilesLoc, alsaUtils, copyq, i3lock-fancy, xmonad }:
''
module Nix.Vars where

dotfilesLocation = "${dotfilesLoc}"

xmonad = "${xmonad}/bin/xmonad"
amixer = "${alsaUtils}/bin/amixer"
copyQ = "${copyq}/bin/copyq"
i3Lock = "${i3lock-fancy}/bin/i3lock-fancy"
''
