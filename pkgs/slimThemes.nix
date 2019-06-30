{ lib, stdenv, fetchFromGitHub, ... }:
let
  outputs = [
    "bluer"
    "bridge"
    "darksome"
    "desky"
    "flat"
    "greeny"
    "hello"
    "material"
    "milkyway"
    "overlay"
    "pinker"
    "sweet_flat"
    "wavy"
    "workspace"
    "boxy"
    "cayny"
    "darky_pink"
    "faded_city"
    "flat_green"
    "greeny_dark"
    "light_red"
    "milk"
    "minimal"
    "panda"
    "pot"
    "typogin"
    "white"
  ];
  mkTheme = output: let
    themeName = lib.escapeShellArg output;
  in stdenv.mkDerivation {
    name = "slim-theme-${output}";
    src = fetchFromGitHub {
      owner = "adi1090x";
      repo = "slim_themes";
      rev = "13cba0d7d6abf286047c3d551d84db415cc7ea52";
      sha256 = "1g5l7hisiv3x49gzvns2lb0z9bn2ac7n67vi1pxasxxlc0l8i14m";
    };
    dontBuild = true;
    dontFixup = true;
    installPhase = ''
      if ! (ls themes/ | grep ^${themeName}$)
      then
        echo Specified theme not found
        exit 1
      fi

      mkdir -p $out
      cp -r themes/${themeName}/* $out
    '';
    meta = with lib; {
      description = "A Beautiful Collection Of SLiM Themes...";
      homepage = "https://github.com/adi1090x/slim_themes#readme";
      license = licenses.unknown;
      platforms = platforms.linux;
      maintainers = [ maintainers.kirens ];
    };
  };
  getOutput = name: {
    inherit name;
    value = mkTheme name;
  };
in lib.listToAttrs (map getOutput outputs)
