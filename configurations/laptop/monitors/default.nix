{ config, pkgs, ... }:
let
  backlight = let
    backlightCtl = pkgs.callPackage ./backlightCtl { };
    light = "${config.security.wrapperDir}/light";
  in lvl: "${light} -S $(${light} | ${backlightCtl} ${lvl})";
in {
  # Modifying backlight seems to require root privileges
  security.wrappers.light.source = "${pkgs.light}/bin/light";

  # Global keybindings
  services.actkbd = {
    enable = true;
    bindings = [
      { keys = [ 224 ]; events = [ "key" ]; command = backlight "-1"; }
      { keys = [ 225 ]; events = [ "key" ]; command = backlight "1"; }
    ];
  };
}
