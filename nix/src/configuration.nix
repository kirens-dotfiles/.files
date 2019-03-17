# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:
let

  myPkgs = import ./nixpkgs { };

in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware/nixpix.x360.3.nix
      ./boot/grub.nix
      ./net/nixpix.nix
      ./users/kiren.nix
      ./virtualization/virtualBox.nix
    ];

  # Suspend to ram and nice stuff
  powerManagement.enable = true;
  # Not sure why I want this
  services.upower.enable = true;

  # Enable CUPS to print documents.
  services.printing = {
    enable = true;
    drivers = with pkgs; [
      myPkgs.cloud-print-connector
      gutenprint
      hplip
      splix
    ];
  };

  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;

    enableDefaultFonts = true;

    fonts = with pkgs; [
#      google-fonts
      font-awesome-ttf

      # Coding
      hack-font
      source-code-pro
      hasklig
      monoid
      iosevka
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
      i3lock-fancy& systemctl suspend
    '';
  };

  # Enable sound.
  # sound.enable = true;
  # hardware.pulseaudio.enable = true;
  # hardware.pulseaudio.support32Bit = true;
}
