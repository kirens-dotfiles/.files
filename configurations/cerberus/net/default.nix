{ ... }:
{
  networking = {
    hostName = "cerberus";
    nameservers = [ "1.1.1.1" "2606:4700:4700::1111" "2606:4700:4700::1001" ]; 

    firewall.allowedTCPPorts = [ 22 80 443 ];
    firewall.allowedUDPPorts = [ ];
  };
}
