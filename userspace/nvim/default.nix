{ config, pkgs, lib, ... }:
let
  inherit (pkgs.callPackage ../lib.nix { })
    importWith
    ;

in {
  options.neovim = {
    # enable = mkEnableOption "Neovim terminal";
    python3 = mkOption {
      type = types.package;
      default = pkgs.python3;
      description = "Python3 to use";
    };
  };

  config = {
    programs.neovim = {
      enable = true;
      viAlias = true;
      configure = importWith ./config.nix pkgs;
    };
  };
}
