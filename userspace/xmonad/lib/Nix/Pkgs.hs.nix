{ alsaUtils, copyq, i3lock-fancy }:
{
  executable = true;
  text = ''
    module Nix.Pkgs where

    amixer = "${alsaUtils}/bin/amixer"
    copyQ = "${copyq}/bin/copyq"
    i3Lock = "${i3lock-fancy}/bin/i3lock-fancy"
  '';
}
