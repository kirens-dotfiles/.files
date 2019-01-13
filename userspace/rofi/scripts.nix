{ stdenv, writeTextFile, bash, xrandr, rofi, xinput, togglAccessToken
, rofi-toggl }:
let
  monitorScript = writeTextFile {
    name = "rofi-monitorScript";
    text = import ./scripts/monitors.nix.sh {
      xrandr = "${xrandr}/bin/xrandr";
      rofi = "${rofi}/bin/rofi";
      xinput = "${xinput}/bin/xinput";
    };
  };
  toggl = writeTextFile {
    name = "rofi-togglScript";
    text = ''
      #! ${bash}/bin/bash

      export TOGGL_TOKEN="${togglAccessToken}"
      ${rofi-toggl}/bin/rofi-toggl
    '';
  };
  scriptSelector = writeTextFile {
    name = "rofi-togglScript";
    text = ''
      #! ${bash}/bin/bash

      cd @scriptsPath@/scripts

      if [[ -z "$@" ]]; then
        find ./ -maxdepth 1 -and -type l -or -type f -printf '%f\n'
      else
        ${bash}/bin/bash "./$1" > /dev/null &
      fi
    '';
  };


in stdenv.mkDerivation {
  name = "rofi-scripts";

  buildCommand = ''
    install -v -D -m755 ${scriptSelector} $out/bin/scripts
    substituteInPlace $out/bin/scripts \
      --subst-var-by scriptsPath "$out"

    install -v -D -m755 ${monitorScript} $out/scripts/monitors
    install -v -D -m755 ${toggl} $out/scripts/toggl
  '';
}
