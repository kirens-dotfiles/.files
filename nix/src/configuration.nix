# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Supposedly better for the SSD
  fileSystems."/".options = [ "noatime" "nodiratime" "discard" ];
  fileSystems."/media/windows" = {
    device = "/dev/sda3";
  };
  fileSystems."/media/e" = {
    device = "/dev/sda5";
#    fsType = "auto";
#    options = [ "remount" "rw" "gid=1000" ];
  };


#  fileSystems = [
#    { mountPoint = "/media/windows";
#      device = "/dev/sda3";
#    }
#    { mountPoint = "/media/e";
#      device = "/dev/sda5";
#    }
#  ];

#  # Use the systemd-boot EFI boot loader.
#  boot.loader.systemd-boot.enable = true;
  boot = {
    loader.grub = {
      enable = true;
      version = 2;
      device = "nodev";
      efiSupport = true; 
      gfxmodeEfi = "1024x768";
    };

    loader.efi.canTouchEfiVariables = true;


    initrd.luks.devices = [
      {
        name = "root";
        device = "/dev/disk/by-uuid/e0b46cd8-62c5-463e-9e21-b33cd7218c23";
        preLVM = true;
        allowDiscards = true;
      }
    ];
  };

 powerManagement.enable = true; 

  networking.hostName = "nixpix"; # Define your hostname.
#  networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "sv-latin1";
    defaultLocale = "en_US.utf8";
  };

  # Set your time zone.
  time.timeZone = "Europe/Stockholm";

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    # sys
    ntfs3g

    # tools
    wget
    unzip
    sudo
    neovim
    gitAndTools.gitFull

    ghc
    haskellPackages.xmobar
    haskellPackages.xmonad
    haskellPackages.xmonad-contrib
    haskellPackages.xmonad-extras

    # X11 stuff
    compton

    feh
#    xclip
    xlibs.xbacklight
    xlibs.xev
    xlibs.xmodmap

    imagemagick7
    mupdf

    vlc

    dmenu

    gimp
    thunderbird
    firefox
    chromium
  ];

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # It's a me
  users.extraUsers.kiren = {
    isNormalUser = true;
    createHome = true;
    home = "/home/kiren";
    description = "Erik Nygren";
    extraGroups = [ "wheel" "kiren" ];
    shell = pkgs.fish;
  };
  users.extraGroups.kiren = {
    gid = 1000; # Will it work without this one?
  };
  security.sudo.enable = true;

  services.upower.enable = true;
  programs.fish.enable = true;


  hardware = {
    opengl = {
      enable = true;
      extraPackages = [ pkgs.vaapiIntel ];
    };

    bluetooth.enable = true;
    pulseaudio = {
      enable = true;
      package = pkgs.pulseaudioFull;
    };
  };

  environment.etc."Xmodmap".text = import ./lib/.Xmodmap.nix { };

  services.xserver = {
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = haskellPackages: [
        haskellPackages.xmonad-contrib
	haskellPackages.xmonad-extras
	haskellPackages.xmonad
      ];
    };
    windowManager.default = "xmonad";
    #videoDrivers = [ "intel" ];
#    vaapiDrivers = [ pkgs.vaapiIntel ];

    enable = true;
    layout = "se";
    desktopManager.xterm.enable = false;
    desktopManager.default = "none";

    displayManager = {
      slim = {
        enable = true;
        defaultUser = "kiren";
	theme = pkgs.fetchurl {
          url = "https://github.com/edwtjo/nixos-black-theme/archive/v1.0.tar.gz";
          sha256 = "13bm7k3p6k7yq47nba08bn48cfv536k4ipnwwp1q1l2ydlp85r9d";
        };
      };
    };
    # Trackpad
    synaptics = {
    	enable = true;
#	dev = "/dev/input/mouse2";
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

  services.compton = {
    enable = true;
  };

   services.redshift = {
     enable = true;
     latitude = "57.687574";
     longitude = "11.979733";
     temperature.day = 6500;
     temperature.night = 3500;
     brightness.day = "1";
     brightness.night = "0.5";
   };
  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "17.09"; # Did you read the comment?

}
