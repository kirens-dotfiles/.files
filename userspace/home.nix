{ lib, pkgs, ... }:
let
  dotfilesLoc = "/e/_FILES/.files";
  name = rec {
    first = "Erik";
    last = "Nygren";
    full = concatStringsSep " " [ first last ];

    userAltUpper = "Kirens";
    userAlt = toLower userAltUpper;
    userUpper = "Kiren";
    user = toLower userUpper;
  };

  email = rec {
    dev = "dev@erik.work";
  };

  # Helpers

  # Imports
  inherit (builtins)
    concatStringsSep
  ;
  inherit (lib)
    toLower
    substring
  ;
  # For packages that need a more up to date channel.
  pkg1809 = import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs-channels/archive/nixos-18.09.tar.gz";
    sha256 = "0hszcsvgcphjny8j0p5inhl45ja61vjiz0csb0kx0b9lzmrafr7b";
  }) {};
in
{
  home.file = {
    ".tmux.conf".source = ./tmux/conf;
    ".config/rofi".source = ./rofi;
    ".config/nixpkgs/config.nix".source = ./config.nix;
    ".config/fish/functions".source = ./fish/functions;
    ".xmonad/scripts/lockptr" =
      import ./scripts/lockptr.nix
        { pkgs = pkgs; };
    ".xmonad/scripts/prettyprints/vol" =
      import ./scripts/prettyprints/vol.nix
        { alsaUtils = pkgs.alsaUtils; };
    ".xmonad/scripts/prettyprints/vol_lvl" =
      import ./scripts/prettyprints/vol_lvl.nix
        { alsaUtils = pkgs.alsaUtils; };
    ".xmonad/scripts/prettyprints/wireless" =
      with pkgs; import ./scripts/prettyprints/wireless.nix {
        wirelesstools = wirelesstools;
        grep = gnugrep;
        awk = gawk;
        coreutils = coreutils;
      };
    ".ghci".text = ''
      :set prompt "λ> "
    '';
  };

  home.packages = with pkgs; [
    btrfsProgs

    curl
    wget
    htop
    # Virtual terminals
    tmux
    spotify
    # Visually manage monitors
    arandr
    # Image viewer
    feh
    # Clipboard interaction CLI
    xclip
    # Detect keypress
    xlibs.xev
    # Adjust buildin display brightness
    xlibs.xbacklight
    # Seems to be needed for some fish completion
    xorg.xwininfo
    # DNS (contains DIG)
    bind


    # For ghci
    ghc

    libreoffice
    atom
    inkscape
    gimp
    vlc
    mupdf
    imagemagick7
    chromium
    firefox
    thunderbird

    # Magnifier
    xzoom
    # Terminal file explorer
    ranger
    # Node without npm
    nodejs-slim
    # Gitkraken only serves latest, so we need newer version
    pkg1809.gitkraken

    # Packages from system that are needed
    bash
    ncurses
    coreutils
    findutils
    nix
    nettools
    gawk
    procps
    networkmanager
    less
    gnused
  ];

  programs = {
    home-manager = {
      enable = true;
      path = "home-manager";
    };

    command-not-found.enable = true;

    git = {
      enable = true;
      userName = name.userAltUpper;
      userEmail = email.dev;

      extraConfig = {
        core = {
          editor = "nvim";
        };
      };
    };

    neovim = {
      enable = true;
      viAlias = true;

      configure = import ./nvim/config.nix { pkgs = pkgs; };
    };

    fish = {
      enable = true;

      inherit (import ./fish/config.fish.nix { dotfilesLoc = dotfilesLoc; })
        shellInit promptInit interactiveShellInit;
    };
  };

  services = {
    redshift = {
      enable = true;
      latitude = "40.642292";
      longitude = "22.879766";

      temperature.day = 6500;
      temperature.night = 3500;
      brightness.day = "1";
      brightness.night = "0.5";

      # For version 1.12 where custom times are enabled
      package = pkg1809.redshift;

      extraOptions = [
        "-c /home/kiren/.config/redshift/redshift.conf"
      ];
    };
  };

  xsession = {
    enable = true;
    windowManager.xmonad = {
      enable = true;

      configDir = ./xmonad;
      configData = with pkgs; xmonad: {
        inherit
          dotfilesLoc xmonad
          alsaUtils copyq i3lock-fancy xautolock rofi libqalculate;
        xmessage = xorg.xmessage;
        xmobar = haskellPackages.xmobar;
        st = pkgs.callPackage ./st/build.nix { };
      };


      enableContribAndExtras = true;
      extraPackages = self: [
      ];
    };
    initExtra = ''
      # Turn off beeps.
      ${pkgs.xorg.xset}/bin/xset -b
      # Switch keyboard layout
      ${pkgs.xorg.xkbcomp}/bin/xkbcomp ${dotfilesLoc}/userspace/keyboard/custom-xkb-keymap :0
    '';
  };

  home.keyboard = {
    layout = "sv";
  };
}
