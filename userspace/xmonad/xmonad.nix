{ callPackage, writeScript, writeTextFile, stdenv, customXmonadGhc, makeWrapper
, buildEnv, runCommand, lib

, alsaUtils, copyq, xautolock, libqalculate, dbus, tmux, playerctl
, imagemagick, st, rofi, translate-shell, xorg
}: { togglAccessToken, multiroomHost, homeDirectory, packages, runtimePath }: let

  absoluteRuntimePath = homeDirectory + toString runtimePath;

  configData = {
    inherit
      alsaUtils
      copyq
      xautolock
      libqalculate
      dbus
      tmux
      playerctl
      imagemagick
      st
      ;

    inherit (xorg) xmessage xbacklight xkbcomp;

    multiroom = (callPackage ../scripts/multiroom {
      inherit  multiroomHost;
    }).package;

    rofi = let
      prgms = buildEnv {
        name = "prgms";
        paths = packages;
      };
    in runCommand
      "xmonad-rofi-with-path"
      { buildInputs = [ makeWrapper ]; }
      ''
        makeWrapper ${rofi}/bin/rofi $out/bin/rofi \
          --set XDG_DATA_DIRS ${prgms}/share \
          --set PATH ${prgms}/bin
      '';

    xmobar = customXmonadGhc.haskellPackages.xmobar;

    rofi-scripts = callPackage ../rofi/scripts.nix {
      inherit togglAccessToken;
      translate-shell = translate-shell;
    };

    custom-keymap = ../keyboard/keymap;

    scripts =
      lib.mapAttrs (name: pac: writeScript name (callPackage pac { })) {
        lockptr = ../scripts/lockptr.nix;
        printVol = ../scripts/prettyprints/vol.nix;
        printVolLvl = ../scripts/prettyprints/vol_lvl.nix;
        wireless = ../scripts/prettyprints/wireless.nix.js;
        randomBackground = ../backgrounds;
      };
  };

  # A file that exposes nix-generated paths to the compilation.
  nixVarsHs = writeTextFile {
    name = "Nix.Vars.hs";
    text = import ./lib/Nix/Vars.nix.hs configData;
  };

in stdenv.mkDerivation {
  name = "custom-xmonad-config";
  src = ./.;
  buildInputs = [ customXmonadGhc makeWrapper ];
  buildPhase = ''
    # Copy Nix.Vars
    ln -s ${nixVarsHs} lib/Nix/Vars.hs
    echo "XMonad is compiling source..."
    ghc --make xmonad.hs -i -ilib -fforce-recomp -main-is main -v0 \
      -o xmonad
    echo "Compilation successfull!"
  '';
  installPhase = ''
    mkdir -p $out/src/lib/Nix
    cp --parents $(find ./ -type f -name "*.hs") $out/src
    ln -s ${nixVarsHs} $out/src/lib/Nix/Vars.hs

    cp xmonad $out/xmonad-x86_64-linux

    # The only runtime reference we want is to where newer restart bins
    makeWrapper $out/xmonad-x86_64-linux $out/xmonad \
      ${lib.optionalString (! isNull runtimePath) ''
        --unset PATH \
        --set XMONAD_BINARY ${absoluteRuntimePath}
      ''}
  '';
}
