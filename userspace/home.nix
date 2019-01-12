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
    concatLists
  ;
  inherit (lib)
    toLower
    substring
    mapAttrsToList
  ;
  # For packages that need a more up to date channel.
  myPkgs = import ../nix/src/nixpkgs { };

  firefoxProfile = "x25cwq9m.default";

  minimalExposure = import ./remapNS.nix { inherit (pkgs) stdenv; };
  specificBins = bins:
    minimalExposure (concatLists [
      (map (n: "bin/${n}") bins)
      (concatLists
        (map (pageNum:
          map (n: {
            from = "share/man/man${pageNum}/${n}.${pageNum}.gz";
            canThrow = false;
          }) bins)
          ["1" "2" "3" "4" "5" "6" "7" "8" "9"]
        )
      )
    ]);
  app = name:
    minimalExposure [
      "share/applications/${name}.desktop"
      { from = "share/${name}"; canThrow = false; }
    ];


  apps = with pkgs; mapAttrsToList app {
    Franz = myPkgs.franz;
    signal-desktop = myPkgs.signal-desktop;
    spotify = spotify;
    # Visually manage monitors
    arandr = arandr;
    atom = atom;
    # Gitkraken only serves latest, so we need newer version
    gitkraken = myPkgs.gitkraken;
  };
  packagesWithSpecificBins = with pkgs;
    map ({ pkg, bins }: specificBins bins pkg) [
      # Image viewer
      { pkg = feh; bins = ["feh"]; }
      # Clipboard interaction CLI
      { pkg = xclip; bins = ["xclip"]; }
      # DNS lookups
      { pkg = { name = "dig-restricted"; outPath = bind.dnsutils.outPath; };
        bins = ["dig"];
      }
      { pkg = ghc; bins = ["ghci"]; }
      { pkg = coreutils; bins = ["cp" "mv" "ls" "rm"]; }
      { pkg = man-db; bins = ["man"]; }
      { pkg = findutils; bins = ["find" "xargs"]; }
      { pkg = nix; bins = ["nix" "nix-shell" "nix-build"]; }
      { pkg = nettools; bins = ["netstat"]; }
      { pkg = networkmanager; bins = ["nmcli"]; }
    ];
in
{
  home.file = {
    ".mozilla/firefox/${firefoxProfile}/chrome" = {
      source = ./firefox/userChrome;
    };
    ".tmux.conf".text =
      import ./shell/tmux/conf
        { sensible = myPkgs.tmuxPlugins.sensible; };
    ".config/rofi".source = ./rofi;
    ".config/nixpkgs/config.nix".source = ./config.nix;
    ".config/fish/functions".source = ./shell/fish/functions;
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



  home.packages = concatLists [
    apps
    packagesWithSpecificBins
    (with pkgs; [
      curl
      wget
      htop

      libreoffice
      inkscape
      gimp
      vlc
      (minimalExposure [{ from = "bin/mupdf-x11"; to = "bin/mupdf"; }] mupdf)
      imagemagick7
      chromium
      firefox
      thunderbird

      # Terminal file explorer
      ranger
      # Node without npm
      nodejs-slim-9_x
    ])
  ];

  programs = {
    home-manager = {
      enable = true;
      path = "home-manager";
    };

    command-not-found.enable = true;

    git = {
      enable = true;
      package = specificBins ["git"] pkgs.git;
      userName = name.userAltUpper;
      userEmail = email.dev;

      extraConfig = {
        core = {
          editor = "nvim";
        };
      };
    };

    neovim  = {
      enable = true;
      package = specificBins ["vi"] myPkgs.neovim;
      viAlias = true;

      configure = import ./nvim/config.nix { pkgs = myPkgs; };
    };

    fish = {
      enable = true;

      inherit
        (import ./shell/fish/config.fish.nix {
          dotfilesLoc = dotfilesLoc;
          powerline = (pkgs.callPackage ./shell/powerline/build.nix {
            powerline-go = myPkgs.powerline-go;
          });
          inherit (pkgs) bash coreutils ncurses;
        })
        shellInit promptInit interactiveShellInit;
    };
  };

  services = {
    #safeeyes = {
    #  enable = true;
    #  package = myPkgs.safeeyes;
    #};
    redshift = {
      enable = true;
      latitude = "40.642292";
      longitude = "22.879766";

      temperature.day = 6500;
      temperature.night = 6500; #3500;
      brightness.day = "1";
      brightness.night = "1"; #"0.5";

      # For version 1.12 where custom times are enabled
      package = myPkgs.redshift;

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
          alsaUtils copyq i3lock-fancy xautolock rofi libqalculate dbus tmux;
        xmessage = xorg.xmessage;
        xmobar = haskellPackages.xmobar;
        st = pkgs.callPackage ./shell/st/build.nix { };
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
