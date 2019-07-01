{ pkgs, stdenv, lib, coreutils }:
let
  inherit (builtins) concatStringsSep listToAttrs concatLists elemAt;

  createCode = arr: fn:
    concatStringsSep "\n" (map fn arr);

  fishDirs = [ "completions" "functions" "test" ];

  createConfigWith = fn:
    listToAttrs (lib.flip map fishDirs (dir: {
      name = ".config/fish/${dir}";
      value = { source = fn dir; };
    }));

  empty = pkgs.writeTextFile { name = "empty"; text = ""; destination = "/empty"; };

  mergeDirFromPackages = pkgs: dir: stdenv.mkDerivation {
    name = "fish-collection-${dir}";
    src = empty;
    buildInputs = [ ];
    buildPhase = "echo Building...";

    installPhase = ''
      mkdir -p $out
      echo Collecting packages into ${dir}...
    '' + createCode pkgs (pkg: ''
      echo '  Collecting files from ${pkg}'
      if [ -d ${pkg}/share/fish/${dir} ]
      then
        for file in $(ls -A ${pkg}/share/fish/${dir})
        do
          echo "  - ${dir}/$file"
          if [ -e "$out/$file" ]
          then
            >&2 echo "Package collision: '${dir}/$file' already exists"
            exit 1
          fi
          cp -rp "${pkg}/share/fish/${dir}/$file" $out
        done
      fi
    '');
  };

in listToAttrs (lib.flip map fishDirs (dir: {
  name = ".config/fish/${dir}";
  value = { source = mergeDirFromPackages (concatLists [
    (import ./packages { inherit pkgs lib stdenv fishDirs createCode; })
    (import ./functions { inherit pkgs lib coreutils; })
  ]) dir; };
}))
