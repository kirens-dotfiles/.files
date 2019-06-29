{ pkgs, config, ... }:
let
  brightnessCtl = pkgs.stdenv.mkDerivation {
    name = "brightnessCtl";
    src = ./.;
    buildInputs = with pkgs; [ ghc ];
    buildPhase = ''
      ghc brightness.hs
    '';
    installPhase = ''
      cp brightness $out
    '';
  };

  light = "${config.security.wrapperDir}/light";

  brightness = lvl: "${light} -S $(${light} | ${brightnessCtl} ${lvl})";

in {
  # It's a me
  users.extraGroups.kiren.gid = 1000;
  users.extraUsers.kiren = {
    isNormalUser = true;
    createHome = true;
    home = "/home/kiren";
    description = "Erik Nygren";
    extraGroups = [ "wheel" "kiren" "docker" "audio" "adbuser" "vboxusers" ];
    shell = pkgs.fish;
  };

  # Home configuration
  home-manager.users.kiren = import ../../../userspace/home.nix;

  # Some stuff I require
  security.sudo.enable = true;
  programs.fish.enable = true;
  programs.adb.enable = true;

  security.wrappers.light.source = "${pkgs.light}/bin/light";
  services.actkbd = {
    enable = true;
    bindings = [
      { keys = [ 224 ]; events = [ "key" ]; command = brightness "-1"; }
      { keys = [ 225 ]; events = [ "key" ]; command = brightness "1"; }
    ];
  };

  # Set your time zone.
  time.timeZone = "Europe/Stockholm";

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "sv-latin1";
#    defaultLocale = "en_AU.UTF-8/UTF-8";
#    supportedLocales = [
#      "en_US.UTF-8/UTF-8" "en_US.utf8" "sv_SE.UTF-8/UTF-8"
#    ];
  };
}
