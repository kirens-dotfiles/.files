{ stdenv, fetchFromGitHub, cmake, pkg-config, vala, gtk3, libgee, webkitgtk
, clutter, clutter-gtk, clutter-gst }:

stdenv.mkDerivation rec {
  name = "komorebi";
  version = "v2.1";

  src = fetchFromGitHub {
    owner = "cheesecakeufo";
    repo = "komorebi";
    rev = version;

    sha256 = "1fdawd13v9z19ycbkv62h34msd0mdqm0bdml05ydjfm6dsi0zw6d";
  };

  configurePhase = ''
    mkdir build && cd build
    cmake ..
  '';



  buildInputs = [
    cmake
    pkg-config
    vala
    gtk3
    libgee
    webkitgtk
    clutter
    clutter-gtk
    clutter-gst
  ];
}
