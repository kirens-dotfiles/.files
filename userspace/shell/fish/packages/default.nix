{ pkgs, stdenv, lib, createCode, fishDirs }:
let
  fishPackage = name: src:
    stdenv.mkDerivation {
      inherit src;
      name = "fish-${name}";
      installPhase = ''
        echo "Installing fish package '${name}'"

        mkdir -p $out/share/fish
        copy() {
          if [ -d $1 ]
          then
            cp -r "$1/" $out/share/fish/
          fi
        }
      '' + (createCode fishDirs (dir: "copy ${dir}"));
    };

in (lib.mapAttrsToList fishPackage {
  fishnet = ./fishnet;
}) ++ [
  (fishPackage "auto-nix-shell" (pkgs.fetchFromGitHub {
    owner = "chrismwendt";
    repo = "auto-nix-shell";
    rev = "86236510daf01ecdc30b7d3b7eb7916aaff3fedf";
    sha256 = "0z3fhzyk0fhnndgc8ckwpvnvad1gn44ijpsm2i475j0a46i5f0sb";
  }))
]
