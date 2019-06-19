{ config, lib, ... }:
let
  localRoute = names: { "127.0.0.1" = names; "::1" = names; };
  mullvad = import ./vpn/mullvad { inherit config lib; };
in {
  networking = {
    hostName = "nixpix";
    nameservers = [ "1.1.1.1" "2606:4700:4700::1111" "2606:4700:4700::1001" ];
    networkmanager.enable = true;

    # Open ports in the firewall.
    firewall.allowedTCPPorts = [ 8080 ];
    firewall.allowedUDPPorts = [ 8080 ];

    hosts =
      lib.zipAttrsWith (lib.const builtins.concatLists) [
        {
          "52.16.245.35" = [ "ombord.sj.se" ];
          "10.101.0.1" = [ "www.ombord.info" ];
        }
        (localRoute [
          "youtube.com" "www.youtube.com"
          "localhost" config.networking.hostName
        ])
      ];
  };
  services.openvpn = mullvad.gothenburg;
}
