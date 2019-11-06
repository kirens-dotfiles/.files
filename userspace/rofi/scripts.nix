{ stdenv, findutils, xrandr, rofi, xinput, togglAccessToken
, rofi-toggl, coreutils, translate-shell, st, tmux, writeScript, gnugrep
, nodejs-slim-10_x, setxkbmap, fetchFromGitHub, writeShellScript
, utillinux }:
let
  translateScript = writeShellScript "rofi-translateScript"
    (import ./scripts/translate.nix.sh {
      rofi = "${rofi}/bin/rofi";
      echo = "${coreutils}/bin/echo";
      test = "${coreutils}/bin/test";
      tail = "${coreutils}/bin/tail";
      translate = "${translate-shell}/bin/trans";
    });

  monitorScript = writeShellScript "rofi-monitorScript"
    (import ./scripts/monitors.nix.sh {
      xrandr = "${xrandr}/bin/xrandr";
      rofi = "${rofi}/bin/rofi";
      xinput = "${xinput}/bin/xinput";
      test = "${coreutils}/bin/test";
      printf = "${coreutils}/bin/printf";
    });

  keyboardLayout = writeScript "rofi-selectLayout"
    (import ./scripts/setxkbmap.nix.js {
      rofi = "${rofi}/bin/rofi";
      node = "${nodejs-slim-10_x}/bin/node";
      setxkbmap = "${setxkbmap}/bin/setxkbmap";
    });

  toggl = writeShellScript "rofi-togglScript" ''
    export TOGGL_TOKEN="${togglAccessToken}"
    ${rofi-toggl}/bin/rofi-toggl
  '';

  scriptSelector = writeShellScript "rofi-selectScript" ''
    cd @scriptsPath@/scripts

    if [[ -z "$@" ]]; then
      ${findutils}/bin/find ./ -maxdepth 1 -and -type l -or -type f -printf '%f\n'
    else
      # NOTE: It seems to be important to redirect streams since a lot of
      #       things will start failing as soon as this process exits and the
      #       streams otherwise would be closed
      ${utillinux}/bin/setsid -f "./$1" > /tmp/null 2> /dev/null &
    fi
  '';

  restoreTmux = writeShellScript "restore-tmux"
    (import ./scripts/restoreTmux.nix.sh {
      st = "${st}/bin/st";
      rofi = "${rofi}/bin/rofi";
      tmux = "${tmux}/bin/tmux";
      grep = "${gnugrep}/bin/grep";
      test = "${coreutils}/bin/test";
    });

  fontawesome  = writeShellScript "fontawesome" (
    import ./scripts/fontawesome.nix.sh {
      inherit fetchFromGitHub;
      printf = "${coreutils}/bin/printf";
      rofi = "${rofi}/bin/rofi";
      cat = "${coreutils}/bin/cat";
      cut = "${coreutils}/bin/cut";
      xclip = "${coreutils}/bin/xclip";
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
    install -v -D -m755 ${keyboardLayout} $out/scripts/keyboard
    install -v -D -m755 ${fontawesome} $out/scripts/fontawesome
  '';
}
