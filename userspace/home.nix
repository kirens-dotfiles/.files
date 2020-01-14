{ config, lib, pkgs, ... }:
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

  # Imports
  inherit (builtins)
    concatStringsSep
    concatLists
  ;
  inherit (lib)
    toLower
    substring
    mapAttrs
    makeBinPath
  ;
  inherit (pkgs)
    callPackage
  ;
  inherit (callPackage ./lib.nix { })
    importWith
  ;

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
      { pkg = fixed-man; bins = ["man"]; }
      { pkg = findutils; bins = ["find" "xargs"]; }
      { pkg = nix; bins = ["nix" "nix-shell" "nix-build" "nix-env" "nix-store"]; }
      { pkg = nettools; bins = ["netstat"]; }
    ];

  fixed-man = (minimalExposure
    [ "bin/man" ]
    (pkgs.man-db.overrideAttrs (o: o // {
      postInstall = with pkgs; ''
        find "$out/bin" -type f | while read file; do
          wrapProgram "$file" --prefix PATH : ${makeBinPath [gzip groff]}
        done
      '';
    }))
  );

  fishFunctions = import ./shell/fish/mergedPackages.nix {
    inherit lib pkgs config;
    inherit (pkgs)
      coreutils
      stdenv
      ;
  };
in {
  home.stateVersion = "18.09";

  imports = [
    ./nvim
    ./xmonad
    ./firefox
  ];

  home.file = fishFunctions // {
    ".tmux.conf".text =
      import ./shell/tmux/conf
        { sensible = pkgs.tmuxPlugins.sensible; };
    ".config/rofi/config".text = pkgs.callPackage ./rofi/config.nix { };
    ".ghci".text = ''
      :set prompt "λ> "
    '';
  };


  xmonad.packages = with pkgs; [
    arandr
    chromium
    gitkraken
    signal-desktop
    spotify
    thunderbird
  ];
  xmonad.runtimePath = /.local/share/xmonad;

  home.packages = concatLists [
    packagesWithSpecificBins
    (with pkgs; [
      coreutils
      curl
      htop
      less

      ag # <- Like grep but for code

      (makeDesktopItem rec {
        name = "CopyQ";
        exec =
          writeScript "CopyQ-Focus" ''
            #! ${bash}/bin/bash
            win=`${xdotool}/bin/xdotool search --class "CopyQ" | ${coreutils}/bin/tail -n 1`
            if ${coreutils}/bin/test "$win" != ""
            then
              ${xdotool}/bin/xdotool windowactivate "$win"
            fi
          '';
        desktopName = name;
        genericName = "Clipboard Manager";
      })

      # Fish completions really needs this
      gawk
      gnused

      inkscape
      gimp
      vlc
      (minimalExposure [{ from = "bin/mupdf-x11"; to = "bin/mupdf"; }] mupdf)

      (callPackage ./devtools/environments { })

      # Node without npm
      nodejs-slim-10_x
    ])
  ];

  home.sessionVariables = {
    EDITOR = "vi";
  };

  programs = {
    command-not-found.enable = true;

    taskwarrior = {
      enable = true;
      # dataLocation = TODO
    };

    man.enable = false;

    git = {
      enable = true;
      package = specificBins ["git"] pkgs.git-customized;
      userName = name.userAltUpper;
      userEmail = email.dev;

      extraConfig = {
        core = {
          editor = "vi";
        };
      };
    };

    fish =
      { enable = true; }
      // import ./shell/fish/config.fish.nix {
        dotfilesLoc = dotfilesLoc;
        powerline = (pkgs.callPackage ./shell/powerline/build.nix {
          powerline-go = pkgs.powerline-go;
        });
        inherit (pkgs) bash ncurses coreutils;
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
    };
  };

  xsession = {
    enable = true;
    initExtra = let
      xset = "${pkgs.xorg.xset}/bin/xset";
      xkbcomp = "${pkgs.xorg.xkbcomp}/bin/xkbcomp";
      xautolock = "${pkgs.xautolock}/bin/xautolock";
      locker = pkgs.stdenv.mkDerivation rec {
        name = "lock-with-betterlockscreen";
        src = pkgs.betterlockscreen;
        buildInputs = [ pkgs.makeWrapper ];
        installPhase = ''
          makeWrapper ${pkgs.betterlockscreen}/bin/betterlockscreen $out \
            --add-flags '--lock blur'
        '';
      };
    in ''
      # Turn off beeps.
      ${xset} -b

      # Faster scrolling with text based controlls
      ${xset} r rate 250 60

      # Switch keyboard layout
      ${xkbcomp} ${./keyboard/keymap} :0

      # Automatic locking
      ${xautolock} -time 5 -locker ${locker} &
    '';
  };

  home.keyboard = {
    layout = "sv";
  };
}
