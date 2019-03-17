{ message, writeTextFile, stdenv, lib, inkscape } :
let
  image = writeTextFile {
    name = "grubSplash.svg";
    text =
      lib.replaceStrings
        [ "{{GRUB_MSG}}" ]
        [ message ]
        (builtins.readFile ./grubSplash.svg);
  };

in stdenv.mkDerivation {
  name = "grubSplash.png";
  src = ./.;
  buildInputs = [ inkscape ];
  installPhase = ''
    inkscape -z -e $out -w 1920 -h 1080 ${image}
  '';
}
