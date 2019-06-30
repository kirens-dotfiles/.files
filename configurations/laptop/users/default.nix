{ config, pkgs, lib, ... }:
{
  imports = [ ./kiren.nix ];

  users.login.path = [ "${pkgs.coreutils}/bin" ];

  services.xserver.displayManager = {
    auto.enable = true;
    auto.user = "kiren";
  };
}
