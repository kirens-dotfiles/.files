{ config, pkgs, lib, ... }:
{
  imports = [ ./kiren.nix ];

  users.login.path = [ "${pkgs.coreutils}/bin" ];

  services.xserver.displayManager.xserverBin =
    lib.mkOverride 11 "${pkgs.xorg.xorgserver.out}/bin/X";
  services.xserver.displayManager.slim = {
    enable = true;
    extraConfig = ''
      login_cmd ${config.services.xserver.displayManager.session.wrapper}
    '';

    theme = pkgs.slimThemes.overlay;
    defaultUser = "kiren";
  };
}
