{ config, pkgs, lib, ... }:
{
  imports = [ ./kiren.nix ];

  services.xserver.displayManager = {
    auto.enable = true;
    auto.user = "kiren";
  };
}
