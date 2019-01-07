{ config, lib, ... }:
{
  networking = {
    hostName = "nixpix";
    nameservers = [ "1.1.1.1" "2606:4700:4700::1111" "2606:4700:4700::1001" ];
    networkmanager.enable = true;

    # Open ports in the firewall.
    firewall.allowedTCPPorts = [ 8080 ];
    firewall.allowedUDPPorts = [ 8080 ];

    hosts =
      lib.zipAttrsWith (lib.const builtins.concatLists) [
        (import ./blockYouTube.nix {})
        (let ds = [ "localhost" config.networking.hostName ];
         in { "127.0.0.1" = ds; "::1" = ds; }
        )
      ];
  };
}
