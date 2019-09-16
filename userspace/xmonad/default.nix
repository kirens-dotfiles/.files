{ config, pkgs, lib, ... }:
let
  cfg = config.xmonad;

  xmonadConfigBuild = pkgs.callPackage ./xmonad.nix { } {
    inherit (config.systemConfig.myCfg) togglAccessToken multiroomHost;
    inherit (config.home) homeDirectory;
    inherit (cfg) packages runtimePath;
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
