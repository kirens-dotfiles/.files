{ pkgs, config, ... }:
{
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
  programs.fish.enable = true;
  programs.adb.enable = true;
}
