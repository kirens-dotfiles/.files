{ pkgs, stdenv }:

stdenv.mkDerivation rec {
  name = "st-custom";
  src = ./src;

  nativeBuildInputs = with pkgs; [ pkgconfig makeWrapper ];
  buildInputs = with pkgs; with xorg; [ libX11 ncurses libXext libXft fontconfig ];

  installPhase = ''
    TERMINFO=$out/share/terminfo make install PREFIX=$out
  '';
}

