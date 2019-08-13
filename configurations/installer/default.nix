{ config, pkgs, ... }:

{
  imports = [
    ../../deps/nixpkgs/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix
  ];
  networking.wireless.enable = false;
  networking.networkmanager.enable = true;
  environment.systemPackages = with pkgs; [
    git networkmanager gparted dotfiles-installer
  ];

  programs.fish.enable = true;

  # Set your time zone.
  time.timeZone = "Europe/Stockholm";

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "sv-latin1";
  };

  services.xserver.enable = true;
  services.xserver.displayManager.startx.enable = true;

  # Create an iso instead of an installer
  entrypoint = config.system.build.isoImage;
}
