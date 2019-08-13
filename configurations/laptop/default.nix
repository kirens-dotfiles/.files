# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:
{
  useEnv = [ ./env.nix ];
  imports =
    [
      # Include the results of the hardware scan.
      ./hardware/nixpix.x360.4.nix

      # Other config packages
      ./audio/bluetooth.nix
      ./boot/grub.nix
      ./graphics/fonts.nix
      ./locale
      ./monitors
      ./net/nixpix.nix
      ./users
    ];

  nix.nixPath = let
    path = toString config.myCfg.dotfilesPath;
  in lib.mkForce [
    "nixpkgs=${path}/deps/nixpkgs"
    "nixos-config=${path}"
  ];

  # Prevent setting path. We want no runtime dependencies, and this will make
  # sure all of them fail
  environment.profileRelativeEnvVars.PATH = lib.mkForce [ ];

  # Don't need or want that many virtual terminals
  services.logind.extraConfig = ''
    NAutoVTs=2
    ReserveVT=2
  '';

  security.sudo.enable = true;

  # Suspend to ram and nice stuff
  powerManagement.enable = true;
  # Not sure why I want this
  services.upower.enable = true;

  # Prevent computer from freezing if memory consuption is too high
  services.earlyoom.enable = true;

  virtualisation.docker.enable = true;
  services.compton.enable = true;
  services.xserver = {
    enable = true;
    layout = "se";

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
      PATH="${pkgs.coreutils}/bin:${pkgs.gawk}/bin:${pkgs.acpi}/bin:$PATH"

      case "$(cat /proc/acpi/button/lid/*/state | awk '{print $2}')" in
        open)
          ;;
        closed)
          if test "$(acpi -a | awk -F ': ' '{print $2}')" != "on-line"
          then
            systemctl suspend
          fi
          ;;
      esac
    '';
  };
}
