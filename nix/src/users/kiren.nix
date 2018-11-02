{ pkgs, ... }:
{
  # It's a me
  users.extraGroups.kiren.gid = 1000;
  users.extraUsers.kiren = {
    isNormalUser = true;
    createHome = true;
    home = "/home/kiren";
    description = "Erik Nygren";
    extraGroups = [ "wheel" "kiren" "docker" "audio" ];
    shell = pkgs.fish;
  };

  # Some stuff I require
  security.sudo.enable = true;
  programs.fish.enable = true;

  # Set your time zone.
  time.timeZone = "Europe/Stockholm";

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "sv-latin1";
    defaultLocale = "en_US.utf8";
  };
}