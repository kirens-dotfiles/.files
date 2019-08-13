{ lib, pkgs, ... }:
let
  dotfilesLoc = "/config";
  name = rec {
    userAltUpper = "Kirens";
  };

  email = rec {
    dev = "dev@erik.work";
  };

  fixed-man = (pkgs.man-db.overrideAttrs (o: {
    postInstall = with pkgs; ''
      find "$out/bin" -type f | while read file; do
        wrapProgram "$file" --prefix PATH : ${lib.makeBinPath [gzip groff]}
      done
    '';
  }));

  callNoOverride = path: args:
    builtins.removeAttrs
      (pkgs.callPackage path args)
      [ "override" "overrideDerivation" ]
    ;

  fishFunctions = import ../../../../userspace/shell/fish/mergedPackages.nix {
    inherit lib pkgs;
    inherit (pkgs)
      coreutils
      stdenv
      ;
  };

in {
  home.stateVersion = "19.03";

  home.file = fishFunctions;

  home.packages = with pkgs; [
    bind.dnsutils
    fixed-man
    findutils
    nix
    nettools

    coreutils
    curl
    htop
    less

    ag # <- Like grep but for code

    # Fish completions really needs this
    gawk
    gnused
  ];

  home.sessionVariables = {
    EDITOR = "vi";
  };

  programs = {
    command-not-found.enable = true;

    man.enable = false;

    git = {
      enable = true;
      package = pkgs.git-customized;
      userName = name.userAltUpper;
      userEmail = email.dev;

      extraConfig = {
        core = {
          editor = "vi";
        };
      };
    };

    neovim = {
      enable = true;
      viAlias = true;
      configure = callNoOverride ../../../../userspace/nvim/config.nix { };
    };

    fish =
      { enable = true; }
      // callNoOverride ../../../../userspace/shell/fish/config.fish.nix {
        inherit dotfilesLoc;
        powerline = (callNoOverride ../../../../userspace/shell/powerline/build.nix {
          powerline-go = pkgs.powerline-go;
        });
      };
  };

  home.keyboard = {
    layout = "sv";
  };
}
