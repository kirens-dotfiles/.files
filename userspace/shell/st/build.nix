{ stdenv, pkgconfig, makeWrapper, xorg, ncurses, fontconfig }:

stdenv.mkDerivation {
  name = "st-custom";
  src = ./src;

  nativeBuildInputs = [ pkgconfig makeWrapper ];
  buildInputs = with xorg; [ ncurses fontconfig libX11 libXext libXft ];

  installPhase = ''
    TERMINFO=$out/share/terminfo make install PREFIX=$out
  '';
}

