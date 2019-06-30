# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:
{
  imports =
    [
      ./env.nix

      # Include the results of the hardware scan.
      ./hardware/nixpix.x360.4.nix

      # Other config packages
      ./audio/bluetooth.nix
      ./boot/grub.nix
      ./locale
      ./monitors
      ./net/nixpix.nix
      ./users/kiren.nix
      ./graphics/fonts.nix
    ];

  # Suspend to ram and nice stuff
  powerManagement.enable = true;
  # Not sure why I want this
  services.upower.enable = true;

  # Prevent computer from freezing if memory consuption is too high
  services.earlyoom.enable = true;

  # Enable CUPS to print documents.
  services.printing = {
    enable = true;
    drivers = with pkgs; [
      gutenprint
      hplip
      splix
    ];
  };

  virtualisation.docker.enable = true;
  services.compton.enable = true;
  services.xserver = {
    enable = true;
    layout = "se";

    displayManager = {
      auto.enable = true;
      auto.user = "kiren";
    };

    # Trackpad
    synaptics = {
      enable = true;
#   dev = "/dev/input/mouse2";
      minSpeed = "0.6";
      maxSpeed = "3";
      accelFactor = "0.1";

      palmDetect = true;
      twoFingerScroll = true;
      additionalOptions = ''
        Option "VertScrollDelta" "-100"
        Option "HorizScrollDelta" "-100"
      '';
    };
  };

  # acpid
  services.acpid = {
    enable = true;
    # Lock & suspend on lid close
    lidEventCommands = ''
      systemctl suspend
    '';
  };
}
