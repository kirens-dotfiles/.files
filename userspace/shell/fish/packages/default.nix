{ stdenv, lib, createCode, fishDirs }:
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
  auto-nix-shell = ./auto-nix-shell;
})
