{ config, pkgs, lib, ... }:
let
  env = import ../env.nix;
  configData = with pkgs; rec {
    inherit
      alsaUtils copyq xautolock libqalculate dbus tmux;

    inherit (xorg) xmessage xbacklight xkbcomp;

    rofi = let
      prgms = buildEnv {
        name = "prgms";
        paths = config.xmonad.packages;
      };
    in runCommand
      "xmonad-rofi-with-path"
      { buildInputs = [ makeWrapper ]; }
      ''
        makeWrapper ${pkgs.rofi}/bin/rofi $out/bin/rofi \
          --set XDG_DATA_DIRS ${prgms}/share \
          --set PATH ${prgms}/bin
      '';

    xmobar = haskellPackages.xmobar;

    st = pkgs.callPackage ../shell/st/build.nix { };

    rofi-scripts = pkgs.callPackage ../rofi/scripts.nix {
      inherit (env) togglAccessToken;
      inherit st;
      rofi-toggl = pkgs.callPackage ../rofi/scripts/toggl
        { nodejs = pkgs.nodejs-slim-10_x; };
      translate-shell = pkgs.translate-shell;
    };

    custom-keymap = ../keyboard/custom-xkb-keymap;

    scripts = with pkgs;
      lib.mapAttrs (name: pac: writeScript name (callPackage pac { })) {
        lockptr = ../scripts/lockptr.nix;
        printVol = ../scripts/prettyprints/vol.nix;
        printVolLvl = ../scripts/prettyprints/vol_lvl.nix;
        wireless = ../scripts/prettyprints/wireless.nix.js;
        randomBackground = ../backgrounds;
      };
  };
  # A file that exposes nix-generated paths to the compilation.
  nixVarsHs = pkgs.writeTextFile {
    name = "Nix.Vars.hs";
    text = import ./lib/Nix/Vars.nix.hs configData;
  };

  xmonadGhc = pkgs.haskellPackages.ghcWithPackages (p: with p; [
    xmonad
    xmonad-contrib
    xmonad-extras
  ]);

  xmonadConfigBuild = pkgs.stdenv.mkDerivation rec {
    name = "custom-xmonad-config";
    src = ./.;
    #env = pkgs.buildEnv { name = name; path = bul; };
    buildInputs = [ xmonadGhc pkgs.makeWrapper ];
    buildPhase = ''
      # Copy Nix.Vars
      ln -s ${nixVarsHs} lib/Nix/Vars.hs
      echo "XMonad is compiling source..."
      ghc --make xmonad.hs -i -ilib -fforce-recomp -main-is main -v0 \
        -o xmonad
      echo "Compilation successfull!"
    '';
    installPhase = ''
      mkdir -p $out/bin $out/src/lib/Nix
      cp --parents $(find ./ -type f -name "*.hs") $out/src
      ln -s ${nixVarsHs} $out/src/lib/Nix/Vars.hs

      cp xmonad $out/bin/xmonad-x86_64-linux
      makeWrapper $out/bin/xmonad-x86_64-linux $out/bin/xmonad
        # We don't want the path
        --unset PATH
    '';
  };

in {
  options.xmonad.packages = lib.mkOption {
    default = [];
    description = "Paths for program launcer";
    type = with lib.types; listOf package;
  };
  config = {
    xsession.windowManager.command = "${xmonadConfigBuild}/bin/xmonad";

    home.activation.XMonad =
    config.lib.dag.entryBetween ["reloadSystemD"] ["installPackages"] ''
    echo "Installing in PATH"
    $DRY_RUN_CMD nix-env -i ${xmonadConfigBuild}

    echo "Sending restart signal"
    {
      $DRY_RUN_CMD xmonad --restart
      echo "Waiting"
      sleep 3
    } || echo "XMonad could not be restarted"

    echo "Removing from PATH"
    $DRY_RUN_CMD nix-env --uninstall ${xmonadConfigBuild}
    '';
  };
}
