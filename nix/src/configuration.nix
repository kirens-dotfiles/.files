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
#  fileSystems."/media/e" = {
#    device = "/dev/sda5";
##    fsType = "auto";
##    options = [ "remount" "rw" "gid=1000" ];
#  };


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
    # Keep up to date to avoid meltdown
    kernelPackages = pkgs.linuxPackages_latest;

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
  networking.nameservers = [ "1.1.1.1" "2606:4700:4700::1111,2606:4700:4700::1001" ];
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
    ## sys
    ntfs3g

    ## tools
    # dig (DNS)
    htop
    bind
    wget
    wirelesstools
    unzip
    sudo
    neovim
    gitAndTools.gitFull
    gnupg
    # pdf tools
    pdftk

    # Terminal web-browser
    elinks

    ghc
    haskellPackages.xmobar
   # haskellPackages.xmonad
   # haskellPackages.xmonad-contrib
   # haskellPackages.xmonad-extras
   ## haskellPackages.X11

    python27Full
    python27Packages.virtualenv
    python3

    gcc
    libffi

    ## X11 stuff
    # Graphical screen config utility
    arandr

    i3lock-fancy
    xautolock
    # Image viewer
    feh
    xclip
    xorg.xwininfo
    xlibs.xbacklight
    # Detect keypress
    xlibs.xev
    # Remaping keys
    xlibs.xmodmap

    # Magnifier
    xzoom

    imagemagick7
    mupdf

    vlc

    dmenu

    # Explorer
    krusader

    gimp
    inkscape
    libreoffice
    thunderbird
    firefox
    chromium
    atom
  ];
  # Fonts :D
  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;

    enableDefaultFonts = true;

    fonts = with pkgs; [
      google-fonts
      font-awesome-ttf

      # Coding
      hack-font
      source-code-pro
      unifont
    ];
  };


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
    extraGroups = [ "wheel" "kiren" "docker" ];
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
#      support32Bit = true;    ## If compatibility with 32-bit applications is desired.
      configFile = "/home/kiren/conf.pa";
      package = pkgs.pulseaudioFull;
    };
  };

  environment.etc."Xmodmap".text = builtins.readFile ./lib/.Xmodmap;

  services.xserver = {
    enable = true;
    layout = "se";

#    desktopManager = {
#      gnome3.enable = true;
#      default = "gnome3";
#    };

    desktopManager.xterm.enable = false;
    desktopManager.default = "none";

    windowManager.default = "xmonad";
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = haskellPackages: [
        haskellPackages.xmonad-contrib
        haskellPackages.xmonad-extras
        haskellPackages.xmonad
      ];
    };
    #videoDrivers = [ "intel" ];
#    vaapiDrivers = [ pkgs.vaapiIntel ];

#    xautolock = {
#        locker = "i3lock-fancy";
#        #nowlocker = "i3lock-fancy";
#        time = 1;
#    };

#     defaultApps = [
#         {mimetypes = ["image/png" "image/jpeg" "image/gif" "image/x-apple-ios-png"]; exec = "${pkgs.feh}/bin/feh";}
# #        {mimetypes = ["text/plain" "text/css"]; exec = "${pkgs.e19.ecrire}/bin/ecrire";}
#         {mimetypes = ["text/html"]; exec = "${pkgs.firefox}/bin/firefox";}
# #        {mimetypes = ["inode/directory"]; exec = "/run/current-system/sw/bin/spacefm";}
#         {mimetypes = ["x-scheme-handler/http" "x-scheme-handler/https"]; exec = "${pkgs.firefox}/bin/firefox";}
# #        {mimetypes = ["application/x-compressed-tar" "application/zip"]; exec = "/run/current-system/sw/bin/xarchiver";}
#     ];

    displayManager = {
      auto.enable = true;
      auto.user = "kiren";
      
#      slim = {
#        enable = false;
#        enable = true;
#        defaultUser = "kiren";
#        theme = pkgs.fetchurl {
#          url = "https://github.com/edwtjo/nixos-black-theme/archive/v1.0.tar.gz";
#          sha256 = "13bm7k3p6k7yq47nba08bn48cfv536k4ipnwwp1q1l2ydlp85r9d";
#        };
#      };
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

  services.compton = {
    enable = true;
  };

  services.redshift = {
    enable = true;
    latitude = "57.687574";
    longitude = "11.979733";
    temperature.day = 6500;
    temperature.night = 6500; #3500;
    brightness.day = "1";
    brightness.night = "5"; #"0.5";
  };
  # acpid
  services.acpid = {
    enable = true;
    # Slock & suspend on lid close
    lidEventCommands = ''
      i3lock-fancy& systemctl suspend
    '';
  };

#  services.openvpn.servers = {
#    testVPN  = { config = '' config vpn/mullvad_se.conf ''; };
#  };

  virtualisation.docker.enable = true;

  # PostgreSQL
  services.postgresql = {
    enable = true;
    package = pkgs.postgresql100;
    enableTCPIP = true;
    port = 5432;

    authentication = pkgs.lib.mkOverride 10 ''
      local all all              trust
      host  all all 127.0.0.1/32 trust
      host  all all 0.0.0.0/0    trust
      host  all all ::1/128      trust
    '';
    extraConfig = ''
      listen_addresses = '*'
    '';

    initialScript = pkgs.writeText "backend-initScript" ''
      CREATE ROLE nixcloud WITH LOGIN PASSWORD 'nixcloud' CREATEDB;
      CREATE DATABASE nixcloud;
      GRANT ALL PRIVILEGES ON DATABASE nixcloud TO nixcloud;

      CREATE ROLE lemmingpants WITH LOGIN PASSWORD 'lemmingpants' CREATEDB;
      ALTER USER lemmingpant CREATEDB;
    '';
  };


  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03"; # Did you read the comment?
}
