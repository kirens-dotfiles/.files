{ lib, writeShellScriptBin, coreutils }: let
  src = lib.cleanSourceWith {
    src = ../../..;
    name = "dotfiles";
    filter = lib.cleanSourceFilter;
  };
in writeShellScriptBin "install-dotfiles" ''
  ${coreutils}/bin/cp -ri ${src} "$1"
''
