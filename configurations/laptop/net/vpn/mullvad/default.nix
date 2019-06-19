{ config, lib, ... }:
let
  server = remotes: {
    servers.primary = {
      autoStart = true;
      updateResolvConf = true;
      authUserPass = {
        username = config.myCfg.mullvadAccountNumber;
        password = "m";
      };
      config = ''
       client
       dev tun
       proto udp

       remote-random
       ${lib.concatMapStringsSep "\n" (r: "remote ${r} 1301") remotes}

       cipher AES-256-CBC
       resolv-retry infinite
       nobind
       persist-key
       persist-tun
       verb 3
       remote-cert-tls server
       ping 10
       ping-restart 60
       sndbuf 524288
       rcvbuf 524288

       fast-io

       ca ${./mullvad_ca.crt}

       tun-ipv6
       tls-cipher TLS-DHE-RSA-WITH-AES-256-GCM-SHA384:TLS-DHE-RSA-WITH-AES-256-CBC-SHA
      '';
    };
  };
  group = name: server ["${name}.mullvad.net"];
in {
  gothenburg = group "se-got";
  stockholm = group "se-sto";
  helsingborg = group "se-hel";
  malmo = group "se-mma";
  london = group "gb-lon";
  new-york = group "us-ny";
  switzerland = group "ch";
}
