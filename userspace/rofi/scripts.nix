{ stdenv, writeTextFile, bash, xrandr, rofi, xinput, togglAccessToken
, rofi-toggl, coreutils, translate-shell, st, tmux, writeScript, gnugrep }:
let
  translateScript = writeTextFile {
    name = "rofi-translateScript";
    text = import ./scripts/translate.nix.sh {
      rofi = "${rofi}/bin/rofi";
      echo = "${coreutils}/bin/echo";
      test = "${coreutils}/bin/test";
      tail = "${coreutils}/bin/tail";
      translate = "${translate-shell}/bin/trans";
      bash = "${bash}/bin/bash";
    };
  };
  monitorScript = writeTextFile {
    name = "rofi-monitorScript";
    text = import ./scripts/monitors.nix.sh {
      xrandr = "${xrandr}/bin/xrandr";
      rofi = "${rofi}/bin/rofi";
      xinput = "${xinput}/bin/xinput";
      test = "${coreutils}/bin/test";
      printf = "${coreutils}/bin/printf";
      bash = "${bash}/bin/bash";
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
        "./$1" > /dev/null &
      fi
    '';
  };
  restoreTmux = writeScript "restore-tmux" (
    import ./scripts/restoreTmux.nix.sh {
      st = "${st}/bin/st";
      rofi = "${rofi}/bin/rofi";
      tmux = "${tmux}/bin/tmux";
      grep = "${gnugrep}/bin/grep";
      test = "${coreutils}/bin/test";
      bash = "${bash}/bin/bash";
    }
  );


in stdenv.mkDerivation {
  name = "rofi-scripts";

  buildCommand = ''
    install -v -D -m755 ${scriptSelector} $out/bin/scripts
    substituteInPlace $out/bin/scripts \
      --subst-var-by scriptsPath "$out"

    install -v -D -m755 ${monitorScript} $out/scripts/monitors
    install -v -D -m755 ${toggl} $out/scripts/toggl
    install -v -D -m755 ${translateScript} $out/scripts/translate
    install -v -D -m755 ${restoreTmux} $out/scripts/restoreTmux
  '';
}
