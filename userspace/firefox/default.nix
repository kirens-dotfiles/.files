{ pkgs, ... }: let
  inherit (pkgs)
    firefox
    ;

  firefoxProfile = "x25cwq9m.default";

in {
  home.file = {
    ".mozilla/firefox/${firefoxProfile}/chrome" = {
      source = ./userChrome;
    };
  };

  xmonad.packages = [ firefox ];
}
