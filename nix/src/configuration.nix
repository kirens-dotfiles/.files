# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./packages.nix
    ];

  boot.extraModulePackages = [ config.boot.kernelPackages.wireguard ];

  # Suspend to ram and nice stuff
  powerManagement.enable = true;
  # Not sure why I want this
  services.upower.enable = true;

  services.printing.enable = true;

  networking.hostName = "nixpix";
  networking.nameservers = [ "1.1.1.1" "2606:4700:4700::1111" "2606:4700:4700::1001" ];
  networking.networkmanager.enable = true;

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "sv-latin1";
    defaultLocale = "en_US.utf8";
  };

  # Set your time zone.
  time.timeZone = "Europe/Stockholm";

  # Some stuff I require
  security.sudo.enable = true;
  programs.fish.enable = true;
  #environment.etc."Xmodmap".text = builtins.readFile ./lib/.Xmodmap;
  # It's a me
  users.extraUsers.kiren = {
    isNormalUser = true;
    createHome = true;
    home = "/home/kiren";
    description = "Erik Nygren";
    extraGroups = [ "wheel" "kiren" "docker" "audio" ];
    shell = pkgs.fish;
  };
  users.extraGroups.kiren = {
    gid = 1000; # Will it work without this one?
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
    # Slock & suspend on lid close
    lidEventCommands = ''
      i3lock-fancy& systemctl suspend
    '';
  };

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 8080 ];
  networking.firewall.allowedUDPPorts = [ 8080 ];


  # VPM
  # Enable Wireguard
  #networking.wireguard.interfaces = {
  #  # "wg0" is the network interface name. You can name the interface arbitrarily.
  #  wg0 = {
  #    # Determines the IP address and subnet of the client's end of the tunnel interface.
  #    ips = [ "10.99.5.254/32" "fc00:bbbb:bbbb:bb01::5fe/128" ];

  #    # Path to the private key file.
  #    #
  #    # Note: The private key can also be included inline via the privateKey option,
  #    # but this makes the private key world-readable; thus, using privateKeyFile is
  #    # recommended.
  #    privateKey = "CEMhH1XaC79zrWq6kzZrXWsReDr0pdsmFjLk61ofanM=";

  #    peers = [
  #      # For a client configuration, one peer entry for the server will suffice.
  #      { #
  #        # Public key of the server (not a file path).
  #        publicKey = "5JMPeO7gXIbR5CnUa/NPNK4L5GqUnreF0/Bozai4pl4=";

  #        # List of IPs assigned to this peer within the tunnel subnet. Used to configure routing.
  #        # For a server peer this should be the whole subnet.
  #        allowedIPs = [ "0.0.0.0/0" "::/0" ];

  #        # Set this to the server IP and port.
  #        endpoint = "185.213.154.130:51820";

  #        # Send keepalives every 25 seconds. Important to keep NAT tables alive.
  #        persistentKeepalive = 25;
  #      }
  #    ];
  #  };
  #};

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.bash.enableCompletion = true;
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;


  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
   sound.enable = true;
   hardware.pulseaudio.enable = true;
   hardware.pulseaudio.support32Bit = true;

  # Enable touchpad support.
  # services.xserver.libinput.enable = true;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03"; # Did you read the comment?

}
