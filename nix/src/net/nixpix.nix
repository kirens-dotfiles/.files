{ ... }:
{
  networking.hostName = "nixpix";
  networking.nameservers = [ "1.1.1.1" "2606:4700:4700::1111" "2606:4700:4700::1001" ];
  networking.networkmanager.enable = true;
  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 8080 ];
  networking.firewall.allowedUDPPorts = [ 8080 ];
}
