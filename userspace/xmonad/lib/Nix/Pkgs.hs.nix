{ dotfilesLoc, alsaUtils, copyq, i3lock-fancy }:
{
  executable = true;
  text = ''
    module Nix.Pkgs where

    dotfilesLocation = "${dotfilesLoc}"

    amixer = "${alsaUtils}/bin/amixer"
    copyQ = "${copyq}/bin/copyq"
    i3Lock = "${i3lock-fancy}/bin/i3lock-fancy"
  '';
}
