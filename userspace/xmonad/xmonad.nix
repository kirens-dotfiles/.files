{ config, pkgs, lib, ... }:
let
  cfg = config.xmonad;
  absoluteRuntimePath = config.home.homeDirectory + toString cfg.runtimePath;

  configData = with pkgs; rec {
    inherit
      alsaUtils copyq xautolock libqalculate dbus tmux;

    inherit (xorg) xmessage xbacklight xkbcomp;

    rofi = let
      prgms = buildEnv {
        name = "prgms";
        paths = cfg.packages;
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
      inherit (config.systemConfig.myCfg) togglAccessToken;
      inherit st;
      rofi-toggl = (pkgs.callPackage ../rofi/scripts/toggl { }).package;
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

  hsPkgsWithOverridenXMonad = pkgs.haskellPackages.override {
    overrides = self: super: {
      xmonad = pkgs.haskell.lib.appendPatch
        super.xmonad ( pkgs.writeText "patch" ''
          diff --git a/src/XMonad/Main.hs b/src/XMonad/Main.hs
          index 70033a3..6486644 100644
          --- a/src/XMonad/Main.hs
          +++ b/src/XMonad/Main.hs
          @@ -418,7 +421,8 @@ handle event@(PropertyEvent { ev_event_type = t, ev_atom = a })
           handle e@ClientMessageEvent { ev_message_type = mt } = do
               a <- getAtom "XMONAD_RESTART"
               if (mt == a)
          -        then restart "xmonad" True
          +        then io (lookupEnv "XMONAD_BINARY")
          +            >>= flip restart True . fromMaybe "xmonad"
                   else broadcastMessage e

           handle e = broadcastMessage e -- trace (eventName e) -- ignoring
        '');
    };
  };
  xmonadGhc = hsPkgsWithOverridenXMonad.ghcWithPackages (p: with p; [
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
      mkdir -p $out/src/lib/Nix
      cp --parents $(find ./ -type f -name "*.hs") $out/src
      ln -s ${nixVarsHs} $out/src/lib/Nix/Vars.hs

      cp xmonad $out/xmonad-x86_64-linux

      # The only runtime reference we want is to where newer restart bins
      makeWrapper $out/xmonad-x86_64-linux $out/xmonad \
        ${lib.optionalString (! isNull cfg.runtimePath) ''
          --unset PATH \
          --set XMONAD_BINARY ${absoluteRuntimePath}
        ''}
    '';
  };

in {
  options.xmonad = with lib; {
    packages = mkOption {
      default = [];
      description = "Paths for program launcer";
      type = with types; listOf package;
    };
    # XXX: This needs to be more rigourus
    runtimePath = mkOption {
      default = null;
      description = "Path pointing to runtime path from where to reload";
      type = with types; nullOr path;
    };
  };
  config = {
    xsession.windowManager.command = "${xmonadConfigBuild}/xmonad";

    home.file."${toString cfg.runtimePath}".source =
      xmonadConfigBuild + /xmonad;

    home.activation.XMonad =
      config.lib.dag.entryBetween ["reloadSystemD"] ["onFilesChange"] ''
        # XXX: This is kind of a hack, but it works in most cases
        tryRestartXMonad() {
          # Assert there is one single X display on a unix socket
          test -d /tmp/.X11-unix || return 1
          test "$(ls /tmp/.X11-unix | wc -l)" = "1" || return 1

          # Set the display in this context
          local DISPLAY=:`ls /tmp/.X11-unix | grep -oP '\d+'`
          # Export the local DISPLAY variable, this will not afect locality
          export DISPLAY

          # Fetch WM info
          local wmInfo=`${pkgs.wmctrl}/bin/wmctrl -m`
          test "$?" = "0" || return 1

          # Assert that the running WM is named xmonad
          (echo "$wmInfo" | grep --quiet 'Name: xmonad') || return 1

          # Try to identify is the same derivation is currently running
          local oldPid=`ps -e | grep xmonad | grep -oP '^\d+'`
          local oldDir=`dirname $(readlink "/proc/$oldPid/exe")`
          if test "$oldDir" = "${xmonadConfigBuild}"
          then
            echo 'No modification to XMonad: not restarting'
            return 0
          fi

          # Perform the actual reloading
          echo "Sending restart signal"
          $DRY_RUN_CMD ${xmonadConfigBuild}/xmonad --restart \
            || return 1
        }

        # Try restarting xmonad
        if ! tryRestartXMonad
        then
          echo 'Warning: Failed to restart xmonad either' 1>&2
          echo '         - a single running X-server was not identified' 1>&2
          echo '         - or the assertion that xmonad is the WM' 1>&2
          echo '         - or the hot-reloading operation failed' 1>&2
        fi
      '';
  };
}
