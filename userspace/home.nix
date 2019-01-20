{ lib, pkgs, ... }:
let
  env = import ./env.nix;
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

  # Imports
  inherit (builtins)
    concatStringsSep
    concatLists
  ;
  inherit (lib)
    toLower
    substring
    mapAttrs
    mapAttrsToList
  ;
  inherit (pkgs)
    callPackage
  ;
  inherit (callPackage ./lib.nix { })
    importWith
  ;
  # For packages that need a more up to date channel.
  myPkgs = import ../nix/src/nixpkgs { };

  firefoxProfile = "x25cwq9m.default";

  minimalExposure = pkgs.callPackage ./remapNS.nix { };
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
      { from = "bin/${name}"; canThrow = false; }
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
      { pkg = coreutils; bins = [
        "cat" "chmod" "chown" "cp" "date" "df" "dirname" "du" "echo" "env"
        "expr" "false" "fmt" "groups" "head" "kill" "ln" "ls" "md5sum" "mkdir"
        "mktemp" "mv" "nohup" "printf" "pwd" "readlink" "rmdir" "seq" "sha1sum"
        "sha256sum" "sha512sum" "sleep" "sort" "tail" "tee" "test" "timeout"
        "touch" "true" "unlink" "uptime" "wc" "yes"
      ]; }
      { pkg = man-db; bins = ["man"]; }
      { pkg = findutils; bins = ["find" "xargs"]; }
      { pkg = nix; bins = ["nix" "nix-shell" "nix-build" "nix-env" "nix-store"]; }
      { pkg = nettools; bins = ["netstat"]; }
    ];

  fishFunctions = import ./shell/fish/functions {
    inherit lib pkgs;
    inherit (pkgs)
      coreutils;
  };
in
{
  home.file = fishFunctions // {
    ".mozilla/firefox/${firefoxProfile}/chrome" = {
      source = ./firefox/userChrome;
    };
    ".tmux.conf".text =
      import ./shell/tmux/conf
        { sensible = myPkgs.tmuxPlugins.sensible; };
    ".config/rofi/config".text = pkgs.callPackage ./rofi/config.nix { };
    ".config/nixpkgs/config.nix".source = ./config.nix;
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
      less

      # Fish completions really needs this
      gawk
      gnused

      libreoffice
      inkscape
      gimp
      vlc
      (minimalExposure [{ from = "bin/mupdf-x11"; to = "bin/mupdf"; }] mupdf)
      imagemagick7
      chromium
      firefox
      thunderbird

      (callPackage ./devtools/environments { })

      # Terminal file explorer
      ranger
      # Node without npm
      myPkgs.nodejs-slim-10_x
    ])
  ];

  programs = {
    home-manager = {
      enable = true;
      path = "home-manager";
    };

    command-not-found.enable = true;

    taskwarrior = {
      enable = true;
      # dataLocation = TODO
    };

    git = {
      enable = true;
      package = specificBins ["git"] pkgs.git;
      userName = name.userAltUpper;
      userEmail = email.dev;

      extraConfig = {
        core = {
          editor = "vi";
        };
      };
    };

    neovim  = {
      enable = true;
      package = specificBins ["vi"] myPkgs.neovim;
      viAlias = true;

      configure = importWith ./nvim/config.nix myPkgs;
    };

    fish =
      { enable = true; }
      // import ./shell/fish/config.fish.nix {
        dotfilesLoc = dotfilesLoc;
        powerline = (pkgs.callPackage ./shell/powerline/build.nix {
          powerline-go = myPkgs.powerline-go;
        });
        inherit (pkgs) bash ncurses;
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
        inherit (xorg) xmessage xbacklight;
        xmobar = haskellPackages.xmobar;
        st = pkgs.callPackage ./shell/st/build.nix { };
        rofi-scripts = pkgs.callPackage ./rofi/scripts.nix {
          rofi-toggl = pkgs.callPackage ./rofi/scripts/toggl
            { nodejs = myPkgs.nodejs-slim-10_x; };
          inherit (env) togglAccessToken;
        };

        scripts = with pkgs;
          mapAttrs (name: pac: writeScript name (callPackage pac { })) {
            lockptr = ./scripts/lockptr.nix;
            printVol = ./scripts/prettyprints/vol.nix;
            printVolLvl = ./scripts/prettyprints/vol_lvl.nix;
            wireless = ./scripts/prettyprints/wireless.nix;
          };
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
